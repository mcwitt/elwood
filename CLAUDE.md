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

## Config Maintenance

When adding or changing a config field in `Config.hs`, update **both** of:

1. **`config.yaml.example`** — must exhaustively document every config option with comments showing defaults and valid values. Every field in `ConfigFile` and its nested types (`CompactionConfigFile`, `PermissionConfigFile`, `MCPServerConfigFile`, `WebhookServerConfigFile`, `WebhookConfigFile`) should have a corresponding commented-out entry.
2. **`modules/assistant.nix`** — the NixOS module must expose a matching option in the `agentModule` and include it in `configContent` generation so it lands in the generated YAML.

## Architecture

- `src/Elwood/App.hs` - Main application wiring
- `src/Elwood/Claude/` - Claude API client and agent loop
- `src/Elwood/Telegram/` - Telegram bot client and polling
- `src/Elwood/Tools/` - Tool implementations
- `src/Elwood/Config.hs` - YAML config parsing
- `src/Elwood/Approval.hs` - Tool approval flow (STM-based coordinator)
