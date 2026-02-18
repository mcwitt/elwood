# Elwood

A self-hosted personal AI assistant that runs as a system service, communicates via messaging platforms, and executes tools on your behalf.

> **Note:** This is an experimental learning project, not intended for production use. It was built as an exercise in Haskell and AI agent architecture.

## Overview

Elwood is inspired by [OpenClaw](https://github.com/openclaw/openclaw) but designed to be minimal, auditable, and tightly integrated with NixOS. The name comes from Claude Shannon's middle name.

**Key features:**

- **Telegram integration** — Chat with your assistant from anywhere
- **Tool execution** — Run commands, read/write files, search the web
- **MCP support** — Extend capabilities with Model Context Protocol servers
- **Persistent memory** — Cross-session knowledge store
- **Proactive scheduling** — Heartbeat checks and cron jobs
- **Tool approval flow** — Approve sensitive operations via inline keyboard
- **Image support** — Send photos and Claude can see them
- **Context compaction** — Automatic summarization for long conversations
- **NixOS module** — Declarative deployment with systemd hardening

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    System Service                       │
│                  (systemd, NixOS)                       │
│                                                         │
│  ┌──────────┐   ┌────────────┐   ┌──────────────────┐  │
│  │ Channels │──▶│   Agent    │──▶│  Tool Dispatch   │  │
│  │(Telegram)│◀──│   Loop     │◀──│  (built-in +     │  │
│  └──────────┘   │            │   │   MCP servers)   │  │
│                 │  ┌───────┐ │   └──────────────────┘  │
│  ┌──────────┐   │  │Session│ │   ┌──────────────────┐  │
│  │Scheduler │──▶│  │Store  │ │   │     Memory       │  │
│  │(heartbeat│   │  │(JSON) │ │   │  (file-based)    │  │
│  │ + cron)  │   │  └───────┘ │   └──────────────────┘  │
│  └──────────┘   └────────────┘                         │
└─────────────────────────────────────────────────────────┘
```

## Building

Elwood is written in Haskell and uses Nix for reproducible builds.

```bash
# Enter development shell
nix develop

# Build
cabal build

# Run tests
cabal test

# Run (requires config.yaml and environment variables)
cabal run elwood
```

## Configuration

Create a `config.yaml` file:

```yaml
# Directory for persistent state (conversations, memory)
stateDir: /var/lib/elwood

# Directory containing SOUL.md, HEARTBEAT.md, etc.
workspaceDir: /var/lib/elwood/workspace

# Telegram chat IDs allowed to interact with the bot
# Find yours by messaging @userinfobot on Telegram
allowedChatIds:
  - 123456789

# Claude model to use
model: claude-sonnet-4-20250514

# Heartbeat/proactive check settings
heartbeat:
  intervalMinutes: 30
  activeHoursStart: 8
  activeHoursEnd: 22

# Permission settings for tools
permissions:
  # Commands that are always allowed (prefix match)
  safeCommands:
    - ls
    - cat
    - git status
    - git log
  # Regex patterns that are always blocked
  dangerousPatterns:
    - "\\brm\\b"
    - "\\bsudo\\b"
  # Paths allowed for file operations
  allowedPaths:
    - workspace
    - "."
  # Per-tool approval policies: allow, ask, or deny
  toolPolicies:
    run_command: ask
    read_file: allow
    write_file: allow
  defaultPolicy: allow
  approvalTimeoutSeconds: 300

# MCP Server configurations (optional)
mcpServers:
  filesystem:
    command: "npx"
    args:
      - "-y"
      - "@modelcontextprotocol/server-filesystem"
      - "/path/to/docs"
```

Set required environment variables:

```bash
export TELEGRAM_BOT_TOKEN="your-bot-token"
export ANTHROPIC_API_KEY="your-api-key"
export BRAVE_SEARCH_API_KEY="your-brave-key"  # optional, for web search
```

## Workspace Files

Place these files in your `workspaceDir`:

- **SOUL.md** — Personality, tone, behavioral guidelines (system prompt)
- **HEARTBEAT.md** — Checklist for proactive monitoring

## NixOS Deployment

Add the flake to your NixOS configuration:

```nix
{
  inputs.elwood.url = "github:mcwitt/elwood";

  outputs = { self, nixpkgs, elwood, ... }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      modules = [
        elwood.nixosModules.default
        {
          services.assistant = {
            enable = true;
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/elwood-env";
            workspaceDir = "/etc/elwood/workspace";
            heartbeat = {
              intervalMinutes = 30;
              activeHoursStart = 8;
              activeHoursEnd = 22;
            };
          };
        }
      ];
    };
  };
}
```

The service runs with systemd hardening (restricted capabilities, protected system paths, etc.).

## Built-in Tools

| Tool | Description |
|------|-------------|
| `run_command` | Execute shell commands (with permission checks) |
| `read_file` | Read files from allowed paths |
| `write_file` | Write files to allowed paths |
| `web_search` | Search the web via Brave Search API |
| `web_fetch` | Fetch and extract text from URLs |
| `save_memory` | Persist knowledge across sessions |
| `search_memory` | Search saved memories |

## Future Ideas

- Additional messaging platforms (Matrix, Discord, etc.)
- Voice message support
- Semantic memory search (vector embeddings)
- Web UI for debugging/administration

## License

MIT
