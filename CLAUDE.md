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

## Architecture

- `src/Elwood/App.hs` - Main application wiring
- `src/Elwood/Claude/` - Claude API client and agent loop
- `src/Elwood/Telegram/` - Telegram bot client and polling
- `src/Elwood/Tools/` - Tool implementations
- `src/Elwood/Config.hs` - YAML config parsing
- `src/Elwood/Approval.hs` - Tool approval flow (STM-based coordinator)
