# Elwood

Personal AI assistant integrating Telegram and Claude.

## Development

```bash
nix develop              # Enter dev shell
cabal build              # Build
cabal test               # Run tests
nix fmt                  # Format and lint
```

## Verification Checklist

Before completing a task:
- [ ] Code compiles: `nix develop -c cabal build`
- [ ] Tests pass: `nix develop -c cabal test`
- [ ] Relevant documentation and examples updated (e.g. README.md, config.yaml.example)
- [ ] Formatted and linted: `nix fmt`
- [ ] Flake checks pass: `nix flake check` (runs build, formatting, weeder, and NixOS module tests)

## Config Maintenance

When adding or changing a config field in `Config.hs`, update all of:

1. **`config.yaml.example`** — must exhaustively document every config option with comments showing defaults and valid values. Cover the keys accepted by `Config`'s parsers and the settings, permissions, provider and webhook parsers they use, including defaults, inheritance and disabling behavior.
2. **`modules/assistant.nix`** — the NixOS module must expose a matching option in the `agentModule` and include it in `configContent` generation so it lands in the generated YAML.
3. **`README.md`** — the config example and NixOS deployment example should reflect the current options. The architecture diagram, feature list, and workspace files section should stay accurate. In particular:
   - The architecture diagram should reflect how components actually interact (e.g. cron scheduling is done by systemd timers that POST to webhook endpoints, not by an internal scheduler).
   - The config example paths should match `config.yaml.example` defaults (e.g. `/var/lib/assistant`).
   - The NixOS deployment example should demonstrate current module options.

## Terminology

- **Turn** = one `runAgentTurn` invocation = human sends a message → agent processes it (possibly calling many tools) → agent returns a response.
- **Iteration** = one step in `agentLoop`, bounded by `maxIterations` = one API call + tool execution within a turn.
- A **turn boundary** in the message history is a user message containing any block other than `ToolResultBlock`, including image-only input. User messages containing only tool results continue the current turn.

## Architecture

- `src/Elwood/App.hs` - Main application wiring
- `src/Elwood/Claude/` - Claude API client and agent loop
- `src/Elwood/Telegram/` - Telegram bot client and polling
- `src/Elwood/Tools/` - Tool implementations
- `src/Elwood/Config.hs` - YAML config parsing
- `src/Elwood/Approval.hs` - Tool approval flow (STM-based coordinator)
