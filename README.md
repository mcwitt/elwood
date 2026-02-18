# Elwood

A self-hosted personal AI assistant that runs as a system service, communicates via messaging platforms, and executes tools on your behalf.

> **Note:** This is an experimental learning project, not intended for production use. It was built as an exercise in Haskell and AI agent architecture.

## Overview

Elwood is inspired by [OpenClaw](https://github.com/openclaw/openclaw) but designed to be minimal, auditable, and tightly integrated with NixOS. The name comes from Claude Shannon's middle name.

**Key features:**

- **Telegram integration** — Chat with your assistant from anywhere
- **Webhook endpoints** — Trigger agent actions from external systems (Home Assistant, n8n, etc.)
- **Tool execution** — Run commands, read/write files, search the web
- **MCP support** — Extend capabilities with Model Context Protocol servers
- **Persistent memory** — Cross-session knowledge store
- **Proactive scheduling** — Heartbeat checks and cron jobs
- **Tool approval flow** — Approve sensitive operations via inline keyboard
- **Image support** — Send photos and Claude can see them
- **Context compaction** — Automatic summarization for long conversations
- **NixOS module** — Multi-agent support with systemd hardening

## Architecture

```
┌───────────────────────────────────────────────────────────┐
│                     System Service                        │
│                   (systemd, NixOS)                        │
│                                                           │
│  ┌──────────┐                                             │
│  │ Telegram │───┐                                         │
│  └──────────┘   │                                         │
│  ┌──────────┐   │  ┌────────────┐   ┌──────────────────┐  │
│  │ Webhooks │───┼─▶│   Event    │──▶│  Tool Dispatch   │  │
│  │ (HTTP)   │   │  │  Handler   │◀──│  (built-in +     │  │
│  └──────────┘   │  │            │   │   MCP servers)   │  │
│  ┌──────────┐   │  │  ┌───────┐ │   └──────────────────┘  │
│  │Scheduler │───┘  │  │Session│ │   ┌──────────────────┐  │
│  │(heartbeat│      │  │Store  │ │   │     Memory       │  │
│  │ + cron)  │      │  │(JSON) │ │   │  (file-based)    │  │
│  └──────────┘      │  └───────┘ │   └──────────────────┘  │
│                    └────────────┘                         │
└───────────────────────────────────────────────────────────┘
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

# Webhook server (optional)
webhook:
  enabled: true
  port: 8080
  globalSecret: "your-webhook-secret"
  endpoints:
    - name: doorbell
      promptTemplate: |
        Motion detected at front door at {{.timestamp}}.
        Please describe what you see.
      session: isolated
      deliver: [telegram]

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
export WEBHOOK_SECRET="your-webhook-secret"   # optional, overrides config file
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
          # Multiple agents can be configured
          services.assistant.agents.elwood = {
            enable = true;
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/elwood-env";
            workspaceDir = "/var/lib/assistant/elwood/workspace";

            heartbeat = {
              enable = true;
              intervalMinutes = 30;
              activeHoursStart = 8;
              activeHoursEnd = 22;
            };

            # Webhook server for external integrations
            webhook = {
              enable = true;
              port = 8080;
              globalSecret = "your-secret";
              endpoints."daily-report" = {
                promptTemplate = "Generate daily report for {{.date}}";
                session = "isolated";
                deliver = [ "telegram" ];
              };
            };

            # Cron jobs via systemd timers
            cronJobs.daily-summary = {
              prompt = "Generate my daily summary";
              useSystemdTimer = true;
              schedule = "08:00";
              isolated = true;
            };

            permissions = {
              safeCommands = [ "ls" "cat" "git status" ];
              dangerousPatterns = [ "\\brm\\b" "\\bsudo\\b" ];
              defaultPolicy = "ask";
            };
          };

          # Run a second agent with different config
          services.assistant.agents.career-coach = {
            enable = true;
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/career-coach-env";
            model = "claude-sonnet-4-20250514";
          };
        }
      ];
    };
  };
}
```

Each agent runs as a separate systemd service (`assistant-<name>.service`) with hardening (restricted capabilities, protected system paths, etc.). Cron jobs with `useSystemdTimer = true` create systemd timers that trigger webhooks.

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
