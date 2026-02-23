# Elwood

A self-hosted personal AI assistant that runs as a system service, communicates via messaging platforms, and executes tools on your behalf.

> **Note:** This is an experimental learning project, not intended for production use. It was built as an exercise in Haskell and AI agent architecture.

## Overview

Elwood is inspired by [OpenClaw](https://github.com/openclaw/openclaw) but designed to be minimal, auditable, and tightly integrated with NixOS. The name comes from Claude Shannon's middle name.

**Key features:**

- **Telegram integration** — Chat with your assistant from anywhere
- **Webhook endpoints** — Trigger agent actions from external systems (Home Assistant, n8n, etc.)
- **Tool execution** — Run commands, read/write files
- **MCP support** — Extend capabilities with Model Context Protocol servers
- **Persistent memory** — Cross-session knowledge store
- **Scheduled tasks** — Cron jobs via systemd timers that call webhooks
- **Tool approval flow** — Approve sensitive operations via inline keyboard
- **Image support** — Send photos and Claude can see them
- **Extended thinking** — Configurable reasoning budget for complex tasks
- **Context compaction** — Automatic summarization for long conversations
- **Dynamic tool loading** — On-demand tool discovery via `find_tools` to reduce per-request token usage
- **Prometheus metrics** — Token usage, API requests, tool calls, and conversation gauges
- **NixOS module** — Multi-agent support with systemd hardening

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                      System Service                          │
│                    (systemd, NixOS)                          │
│                                                              │
│  ┌──────────┐                                                │
│  │ Telegram │───┐                                            │
│  │(polling) │   │                                            │
│  └──────────┘   │  ┌────────────┐   ┌───────────────────┐    │
│  ┌──────────┐   ├─>│   Event    │──>│  Tool Dispatch    │    │
│  │ Webhooks │───┘  │  Handler   │<──│  (built-in +      │    │
│  │ (HTTP)   │      │            │   │   MCP servers)    │    │
│  └──────────┘      │  ┌───────┐ │   └───────────────────┘    │
│       ^            │  │Session│ │   ┌───────────────────┐    │
│       │            │  │Store  │ │   │     Memory        │    │
│       │            │  │(JSON) │ │   │  (file-based)     │    │
│       │            │  └───────┘ │   └───────────────────┘    │
│       │            └────────────┘                            │
│       │                                                      │
│  ┌────┴─────────────────────────────┐                        │
│  │  systemd timers (cron jobs)      │                        │
│  │  POST to webhook endpoints       │                        │
│  └──────────────────────────────────┘                        │
└──────────────────────────────────────────────────────────────┘
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

Create a `config.yaml` file (see [`config.yaml.example`](config.yaml.example) for all options):

```yaml
stateDir: /var/lib/assistant
workspaceDir: /var/lib/assistant/workspace

allowedChatIds:
  - 123456789

model: claude-sonnet-4-20250514
thinking: off  # off | {type: adaptive, effort: medium} | {type: fixed, budgetTokens: 4096}

compaction:
  tokenThreshold: 50000
  compactionModel: claude-3-5-haiku-20241022

permissions:
  defaultPolicy: allow  # allow | ask | deny
  approvalTimeoutSeconds: 120
  toolPolicies:
    run_command: ask
    write_file: ask
  dangerousPatterns:
    - "\\brm\\b"
    - "\\bsudo\\b"
  safePatterns:
    - "^rm -i\\b"

webhook:
  enabled: true
  port: 8080
  globalSecret: "your-webhook-secret"
  endpoints:
    - name: doorbell
      prompt:
        - type: text
          content: |
            Motion detected at front door at {{.timestamp}}.
            Please describe what you see.
      deliver:
        - type: telegram

mcpServers:
  filesystem:
    command: npx
    args:
      - "-y"
      - "@modelcontextprotocol/server-filesystem"
      - "/path/to/docs"
```

Set required environment variables:

```bash
export TELEGRAM_BOT_TOKEN="your-bot-token"
export ANTHROPIC_API_KEY="your-api-key"
export WEBHOOK_SECRET="your-webhook-secret"   # optional, overrides config file
```

## Workspace Files

Place these files in your `workspaceDir`:

- **SOUL.md** — Personality, tone, behavioral guidelines (system prompt, loaded by default)

The system prompt, webhook prompts, and cron job prompts are all configured as lists of prompt inputs. Each input is either a `workspaceFile` (read from `workspaceDir`) or inline `text`. Webhook text inputs support `{{.field}}` template placeholders for dynamic content from the JSON payload.

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
          services.assistant.agents.elwood = {
            enable = true;
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/elwood-env";
            workspaceDir = "/var/lib/assistant/elwood/workspace";

            webhook = {
              enable = true;
              port = 8080;
              globalSecret = "your-secret";
              endpoints."doorbell" = {
                prompt = [ { type = "text"; content = "Motion detected at {{.timestamp}}"; } ];
                deliver = [ { type = "telegram"; } ];
              };
            };

            # Cron jobs are systemd timers that POST to auto-generated webhook endpoints
            cronJobs.heartbeat = {
              prompt = [ { type = "text"; content = "Check system health. Reply HEARTBEAT_OK if all is well."; } ];
              schedule = "*-*-* *:00/30";  # every 30 minutes
              session = "123456789";       # share conversation with Telegram chat
              deliver = [ { type = "telegram"; session = "123456789"; } ];
              suppressIfEquals = "HEARTBEAT_OK";
            };

            cronJobs.daily-summary = {
              prompt = [ { type = "text"; content = "Generate my daily summary"; } ];
              schedule = "*-*-* 08:00";
              deliver = [ { type = "telegram"; } ];  # broadcast (default)
              # session = null (default); each run is isolated
            };

            mcpServers.filesystem = {
              command = "npx";
              args = [ "-y" "@modelcontextprotocol/server-filesystem" "/var/lib/assistant/elwood/workspace" ];
            };

            permissions = {
              dangerousPatterns = [ "\\brm\\b" "\\bsudo\\b" ];
              safePatterns = [ "^rm -i\\b" ];
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

Each agent runs as a separate systemd service (`assistant-<name>.service`) with hardening (restricted capabilities, protected system paths, etc.). Cron jobs create systemd timers that trigger auto-generated webhook endpoints.

## Built-in Tools

| Tool | Description |
|------|-------------|
| `run_command` | Execute shell commands (with permission checks) |
| `save_memory` | Persist knowledge across sessions |
| `search_memory` | Search saved memories |
| `queue_attachment` | Queue files to send as Telegram attachments |

## Monitoring

When the webhook server is enabled, a Prometheus-compatible metrics endpoint is available at `/metrics`. No authentication is required for this endpoint.

**Available metrics:**

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `elwood_input_tokens_total` | counter | model, source | Input tokens consumed |
| `elwood_output_tokens_total` | counter | model, source | Output tokens consumed |
| `elwood_cache_read_tokens_total` | counter | model, source | Cache read tokens |
| `elwood_cache_creation_tokens_total` | counter | model, source | Cache creation tokens |
| `elwood_api_requests_total` | counter | model, source, stop_reason | API requests made |
| `elwood_tool_calls_total` | counter | tool | Tool invocations |
| `elwood_compactions_total` | counter | — | Conversation compactions |
| `elwood_conversation_messages` | gauge | session | Messages per conversation |
| `elwood_conversation_estimated_tokens` | gauge | session | Estimated tokens per conversation |
| `elwood_tools_registered` | gauge | — | Number of registered tools |
| `elwood_mcp_servers_active` | gauge | — | Number of active MCP servers |

**Example Prometheus scrape config:**

```yaml
scrape_configs:
  - job_name: elwood
    static_configs:
      - targets: ['localhost:8080']
    metrics_path: /metrics
```

## Future Ideas

- Additional messaging platforms (Matrix, Discord, etc.)
- Voice message support
- Semantic memory search (vector embeddings)
- Web UI for debugging/administration

## License

MIT
