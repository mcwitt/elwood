# Design and behavioral contracts

Elwood uses GHC2024. Its core describes conversation turns as immutable values; small IO handles interpret those values and own external resources.

## Turn model

A conversation is an ordered sequence of turns. A turn consists of an input, zero or more model/tool exchanges, and a terminal outcome. Its observable trace contains messages and tool results in order. Trace concatenation is associative with the empty trace as identity; interpreting concatenated traces preserves that order. The conversation interpreter also depends on session state.

`Elwood.Claude.Turn` parses model replies into completion decisions or validated, nonempty batches of tool calls. Each call has a nonempty ID and name and an object-valued input. Traversing a batch pairs each call with exactly one result in call order. The IO interpreter owns provider requests, tool execution, approval and delivery.

* `tool_use` requires nonempty, uniquely identified client calls. `end_turn` with client calls is an error. Unknown stop states are errors.
* `max_tokens` produces a truncated outcome and excludes incomplete tool calls from execution and persistence.
* Signed thinking and opaque provider blocks survive replay. Unknown blocks are never executable.
* A user message containing any block other than a tool result starts a turn, including image-only input. Provider-specific image hoisting applies only to outgoing HTTP requests.
* Request and tool-call counts belong to the current turn. Failed or malformed compaction preserves the conversation. Pruning preserves message order and tool-result pairing without changing durable history.

## Composition and input contracts

`Elwood.Tools.Builder` combines a schema, input parser and action. Parse failures become tool errors. Registries compose by name with right-biased replacement, allowing context-sensitive tools to be installed. MCP catalog assembly rejects duplicate final names.

Configuration overrides compose associatively and resolve once at the input boundary. Integer parameters reject fractions and overflow. `Positive` provides a total smart constructor and checked type-level literals. Permission patterns compile during parsing. Tool-name arrays reject nonstrings. Memory keys are accepted unchanged or rejected; distinct accepted keys cannot alias on disk.

## Lifetimes and concurrency

Named sessions admit turns in FIFO order, with at most one active turn per session. Independent sessions can run concurrently. A reply's tools execute concurrently through the `Concurrently` Applicative, while the traversal retains call/result pairing and declared order. Dependent operations require separate replies. Asynchronous delegation starts background work that can outlive its initiating reply.

Each tool batch is scoped: turn cancellation or an asynchronous exception cancels its running calls and waits for their cleanup. A synchronous tool exception becomes that call's error result without cancelling its siblings. Each active turn owns a fresh cancellation token. An idle session reports no active turn to cancel. `/stop` cancels the active turn and its delegates without affecting queued successors or unrelated sessions. `/clear` participates in session serialization. Approval waits observe turn cancellation and release their registrations on exit.

`Elwood.Workers` owns background jobs and FIFO named lanes. Task registration and shutdown share an ownership boundary. Synchronous exceptions may become domain errors; asynchronous cancellation propagates. Shell commands and MCP transports own their child processes and clean them up when their scope exits.

Completed delegate results support repeated reads for a retention period measured from completion. Execution deadlines are independent of retention. Explicit cancellation removes a task and waits for worker shutdown. Background delegates return artifact paths through retained results; synchronous delegates can also use the turn's attachment queue and progress notifications.

MCP serializes complete frames on stdin. Reader exit closes the transport, fails pending requests and rejects new requests. Timeout and cancellation remove pending entries. Interrupted or failed writes close the transport.

## Storage and delivery

Conversation and callback stores serialize updates and publish in-memory state only after successful atomic file replacement. In-memory and file-backed conversation stores interpret the same update functions. Corrupt persisted data is an error. Atomic replacement provides atomic visibility, not power-loss durability.

Conversation files use chunked UTF-8 hex paths under `conversations/v2/`. When an encoded file is absent, `Elwood.Compat.Conversation` reads flat files under `conversations/` and checks the embedded session ID. The store writes the imported conversation to its encoded path and leaves the source intact. A filename collision is an error.

Due callbacks are removed from persistent pending work before dispatch. This provides at-most-once claiming in a live process; a crash between removal and dispatch can lose a callback. External effects are not exactly-once.

`Event.Runner` and the webhook handle separate admission from execution and delivery. Suppression controls ordinary narration, final text and attachments independently of delivery destination. Scheduled callbacks capture that destination. Explicit `send_message` calls deliver immediately.

The boundary handles follow the [handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html): keep pure logic outside IO, pass small dependencies explicitly, and bracket resource lifetimes. Tests specify protocol decisions, algebraic laws, persistence, cancellation, process lifetimes and adapter behavior.
