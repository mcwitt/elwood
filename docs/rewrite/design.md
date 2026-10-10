# Elwood rewrite: design and behavioral specification

Baseline: commit `a0355507af9d43b8a6777f6c91b818debf9b29d9`, GHC 9.10.3, GHC2024. The existing tests describe supported external behavior, not a requirement to retain internal APIs. Production SLOC includes src/ and app/, excludes tests, and is measured by scripts/haskell-sloc.py after normal formatting. The target is at most half the baseline.

## Denotation

A conversation is an ordered sequence of turns. A turn consists of an input followed by zero or more model/tool exchanges and a terminal outcome. Its meaning is an ordered trace of observable messages and tool results. Concatenation of traces is associative with the empty trace as identity; interpreting concatenation must sequence the interpretations in the same order.

The core is a pure transition system. A model reply is parsed into a decision: Finish, Continue (a nonempty collection of typed tool calls), or Reject (a protocol error). A tool call has an ID, name, and JSON input; arbitrary content blocks cannot masquerade as calls. A continuation preserves the assistant reply and pairs each call with exactly one result in call order. Termination distinguishes completion, truncation, exhaustion, cancellation, and provider failure. The iteration budget counts model requests in this turn, and exhaustion statistics count only this turn's calls.

The IO interpreter owns requests, tool execution, approval, and delivery. Dependencies are small records of functions (handles); pure decisions do not read mutable environment records. This takes inspiration from the handle pattern without adding an effect framework. Tools are schema plus a parser and an action; a shared constructor interprets parse errors as tool errors. Registries compose by name and can be restricted before execution. Configuration overrides compose associatively, then resolve once at the boundary.

## Lifetimes and storage

One named session has at most one active turn. Cancellation belongs to the active turn, not a queued successor. Independent sessions may run concurrently. Model-requested tools execute in response order by default: tools include stateful shell commands and must not be speculatively parallelized. Explicit delegate tasks retain concurrency.

Conversation and callback stores serialize read/modify/persist within one ownership boundary. File updates use atomic replacement; publish in-memory state only after a successful atomic file replacement. In-memory and file-backed conversation stores interpret the same update functions. Names must not silently alias on disk. Background workers have scoped ownership and exception-safe shutdown. Synchronous exceptions may become domain errors; asynchronous cancellation must propagate.

Async delegate results remain readable until expiry or explicit cancellation; observation must not consume a result. Expiry must never silently discard a running task. Scheduling guarantees at-most-once claiming in a live process, not exactly-once external effects across crashes. Persist removal before firing, and document this limitation.

## Features retained

Telegram polling and commands, Markdown delivery and chunking, inbound attachment archival and image perception, webhook authentication/templates, named and isolated sessions, permissions and interactive approvals, command/memory/image/attachment/message tools, synchronous and asynchronous delegation, structured output, provider routing, cache controls, context compaction and pruning, MCP stdio tools including image results, callbacks, metrics, YAML configuration and NixOS deployment.

## Intentional simplifications to encode in tests before implementation

1. Execute a reply's tools sequentially in declared order; explicit delegation remains the concurrency primitive.
2. Reject fractional or out-of-range integer tool parameters instead of rounding or overflowing.
3. Reject invalid memory keys instead of deleting characters/truncating and aliasing distinct keys.
4. Task polling/awaiting observes rather than consumes completed results; repeated reads are stable until expiry/cancellation.
5. Unknown or malformed model stop states are errors, never silently successful answers. Wire-format unknown content preservation is retained for provider forward compatibility where safe.
6. Remove incidental prose/exact formatting requirements from tests only where not a wire contract. Preserve stable structured errors, JSON shapes, permission rules and actual delivery outcomes.
7. Replace filename sanitization with a collision-free encoding for conversation IDs (with an explicit legacy migration path or documented migration).

No feature is to be removed merely to reach the size target. Tests covering real behavior must remain or be strengthened; new tests cover algebraic laws, protocol decisions, persistence, cancellation and concurrency. Existing config compatibility is preferred over needless migration.

## Implementation partitions

A. Domain and model protocol: typed turn core, Claude types/client/loop/compaction/pruning/observer, configuration/settings/provider/thinking/permissions/positive types. Aim for approximately 45% of original SLOC.
B. Tools and state: tool combinators/implementations, delegation/tasks, MCP, scheduler, conversation persistence, session/approval, memory/command/image. Aim for approximately 45% of original SLOC.
C. Application adapters: event interpretation, application wiring, Telegram, webhooks, metrics, logging, notification, prompts and facade modules. Aim for approximately 45% of original SLOC.

Independent design reviewers must evaluate these boundaries and contracts before implementation. Final independent reviewers must assess correctness, idiomatic modern Haskell, usability and maintenance; every finding receives a fix or reasoned refutation before consensus.

## Review refinements and concrete contracts

* Persistence promises atomic visibility and serialization, not power-loss durability (no fsync claim). Corrupt persisted data is an error, never silently replaced with an empty store. Legacy conversation paths may be imported only if the embedded session ID exactly matches the requested ID.
* Protocol acceptance: tool_use requires nonempty, uniquely identified client calls. end_turn with client calls is rejected. max_tokens preserves a truncated answer but never executes incomplete calls. Unknown stop states are errors. Calls and results are paired structurally, never zipped from separately sized lists. Original signed thinking and opaque provider blocks must survive replay.
* A human input is any user message that is not purely tool results; image-only inputs count as turn boundaries. Provider-specific image hoisting happens only in the wire projection, so it cannot alter stored turn boundaries. A typed turn records the delta and counts current-turn requests/calls; history is not part of those statistics.
* Failed or malformed compaction preserves the original history. Pruning never mutates durable conversation history and preserves role/order and tool-result pairing.
* Cancel applies only to the active turn. An idle session reports False. Allocate a fresh token on admission and clear it on release, including exceptional release. /clear participates in the same session serialization. /stop must cancel delegates owned by that turn/session, never unrelated sessions.
* Background delegates do not inherit a turn-local attachment queue or buffered progress callback, because their lifetime can exceed that turn. They return artifact paths in retained task results; synchronous delegates retain those tools and notifications.
* Async tasks are created and registered under store ownership, with cleanup if registration is interrupted. Completed results remain available for a full retention TTL measured from completion; execution deadlines are separate. Check and await share parsing and result interpretation. Explicit cancel removes a task and waits for worker shutdown. All tasks have a shutdown owner.
* MCP publishes a closed state when its reader exits, rejects new requests after closure, completes pending requests with transport failure, and cleans pending entries on timeout or cancellation. Stdin writes serialize complete frames. Each child process has scoped cleanup.
* Suppression is a delivery policy independent of delivery destination. Suppressed turns still capture the original callback destination. send_message is explicitly eager; ordinary narration/final text/attachments obey suppression.
* Validate integer parameters without rounding, range overflow, or partial Num instances. Positive has a total smart constructor and a checked type-level literal constructor. Tool-name arrays reject nonstrings rather than dropping them. Invalid memory keys are rejected unchanged, never sanitized into aliases.
* Retain configuration and wire compatibility where it represents a feature. Parse defaulting and overrides centrally. Keep static protocol ADTs explicit; generic JSON helpers are appropriate for ordinary records, not a way to obscure wire contracts.

The SLOC target is a constraint on the whole implementation, not justification to minify, drop features, weaken assertions or move production logic into tests or another language.

## Implemented interfaces

`Elwood.Claude.Turn` owns validated `Pending` batches, object-valued `ToolCall`s, paired `Exchange`s and pure completion decisions. `AgentLoop` interprets this model with existing HTTP, tool and observation handles. The algebra applies to ordered output traces; a complete conversation interpreter is state-dependent, not itself claimed to be a simple monoid homomorphism.

`Elwood.Tools.Builder` interprets a tool's schema, input parser and action. `Elwood.Persistence` supplies atomic replacement; each store owns mutation serialization. `Elwood.Workers` owns background jobs and FIFO named lanes. `Event.Runner` and the webhook handle separate adapter admission from execution and delivery policy. Session tokens identify an active turn; tasks carry both session visibility and their originating turn token.

Explicit registry replacement is right-biased (used to rebuild context-sensitive tools). MCP catalog assembly rejects accidental duplicate final names. Legacy conversation import requires an exact embedded identity match; new paths use chunked UTF-8 hex under `conversations/v2`.

The boundary handles follow the principles in [Jasper Van der Jeugt's handle-pattern article](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html): keep pure logic outside IO, pass small dependencies explicitly, and bracket resource lifetimes.
