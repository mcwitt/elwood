# Independent review record

Three agents reviewed the design independently, then implemented separate partitions. Each also cross-reviewed code authored by another agent. Root coordinated integration and verified the full suite. Reviews assessed correctness, modern idiomatic Haskell, usability and maintenance. Acceptance required fixes or reasoned refutations for every finding.

## Design review

All three reviewers accepted the revised contracts in design.md. Required refinements included a protocol acceptance matrix, structurally paired exchanges, fresh turn cancellation tokens, atomic persistence semantics, independent task retention/deadlines, MCP closure rules, FIFO session admission, and delivery policy separate from destination.

## Findings addressed during implementation

| Finding | Resolution |
| --- | --- |
| Shell pipe deadlock and surviving descendants | Concurrent pipe reads; masked process-group acquisition and cleanup; lifetime regressions |
| Partial Positive Num and rounded numeric input | Total runtime smart constructor; checked type-level literals; strict integer parsing |
| Invalid thinking budgets constructible | Thinking Budget carries Positive |
| Retention and pruning arithmetic overflow | Bound counts before narrowing and use Integer for length arithmetic |
| Corrupt supported image data passed perception | Decode failures rejected; WebP passthrough remains explicit |
| Incomplete tool calls persisted on truncation | Terminal projection excludes unexecuted incomplete calls |
| Failed compaction deletes history | Original history retained unless a complete nonempty summary succeeds |
| Queued worker cancellation leaves lane blocked | Lane reservation and child registration share one ownership boundary |
| Stop cancels successor/unrelated delegates | Tasks carry originating turn token; admission rejects cancelled turns |
| Synchronous delegate ignores parent cancellation | Delegate interpreter reads originating turn token |
| Retention doubles as execution deadline | Independent background execution default and completion-based retention |
| Legacy histories disappear / long IDs exceed filename limits | Identity-checked legacy import and chunked collision-free paths |
| Duplicate MCP names silently replace tools | Reject duplicate catalog names before registration |
| Interrupted frame leaves MCP stream corrupt | Fail closed and terminate transport on interrupted/failed write |
| Retry backoff ignores cancellation | Cooperative cancellation during backoff; cancelled turns issue no further request |
| Approval wait prevents stopping a turn | Race approval against the originating turn token and bracket registration cleanup |
| Background delegate outlives attachment/progress handles | Background tasks return artifact paths through retained results; synchronous delegates retain turn-local delivery |
| Scheduled callback failures disappear in workers | Log provider errors and persistence exceptions inside the worker action; regression tests cover both |
| Configuration tests compare implementation-shaped intermediate records | Test actual parsed and resolved defaults, nulls, disabling, inheritance, destinations and invalid inputs |

## Final verification

* All 629 tests pass, including protocol, algebraic laws, persistence, cancellation, process lifetime, transport and adapter regressions. The baseline had 548 passing tests.
* `nix fmt` passes: cabal-fmt, architecture-diagram generation, HLint, nixfmt and Ormolu.
* `nix flake check --no-write-lock-file` passes on x86_64-linux, including the package build, Weeder and all seven NixOS VM checks. Other platforms were not checked.
* `git diff --check` passes.
* Production Haskell SLOC is 4,340, down from 8,709 at baseline commit `a0355507af9d43b8a6777f6c91b818debf9b29d9`: a 50.2% reduction. Counts include src/ and app/, exclude tests/comments/blank lines, and use ordinary formatted source. Reproduce with `python3 scripts/haskell-sloc.py` and `python3 scripts/haskell-sloc.py --revision a0355507af9d43b8a6777f6c91b818debf9b29d9`.

## Final independent consensus

| Reviewer | Independently reviewed areas | Decision |
| --- | --- | --- |
| review_domain | Configuration, persistence, sessions, scheduler, MCP and tools | Accepted; no unresolved findings |
| review_state | Application and delivery adapters | Accepted; no unresolved findings |
| review_adapters | Claude core and configuration/image boundaries | Accepted; no unresolved findings |

All three reviewers confirmed final acceptance after receiving the integrated verification results. Each assessed correctness, modern idiomatic Haskell, usability and maintenance. Intentional behavioral changes and remaining persistence/delivery guarantees are documented in [design.md](design.md) and the README.
