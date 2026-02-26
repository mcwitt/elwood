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
- **Tool approval flow** — Approve sensitive operations via inline keyboard (Telegram only; webhook-triggered runs deny `ask` tools)
- **Image support** — Send photos and Claude can see them
- **Extended thinking** — Configurable reasoning budget for complex tasks
- **Context compaction** — Automatic summarization for long conversations
- **Server-side tool search** — On-demand tool discovery via Anthropic's tool search with deferred loading
- **Typing indicator** — Shows "typing..." in Telegram while the agent works
- **Cost tracking** — Approximate API cost metric via model-aware pricing
- **Prometheus metrics** — Token usage, API requests, tool calls, and conversation gauges
- **NixOS module** — Multi-agent support with systemd hardening

## Architecture

<img src="docs/architecture.svg" width="450" alt="Architecture">

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
state_dir: /var/lib/assistant
workspace_dir: /var/lib/assistant/workspace

allowed_chat_ids:
  - 123456789

system_prompt:
  - type: workspace_file
    path: SOUL.md

model: claude-sonnet-4-20250514
thinking: off  # off | {type: adaptive, effort: medium} | {type: fixed, budget_tokens: 4096}

compaction:
  token_threshold: 50000
  model: claude-3-5-haiku-20241022

permissions:
  default_policy: allow  # allow | ask | deny
  approval_timeout_seconds: 120
  tool_policies:
    run_command: ask  # Telegram prompts for approval; webhooks deny (use "allow" for webhook-safe tools)
  dangerous_patterns:
    - "\\brm\\b"
    - "\\bsudo\\b"
  safe_patterns:
    - "^rm -i\\b"

webhook:
  enabled: true
  port: 8080
  global_secret: "your-webhook-secret"
  endpoints:
    - name: doorbell
      prompt:
        - type: text
          content: |
            Motion detected at front door at {{.timestamp}}.
            Please describe what you see.
      delivery_targets:
        - type: telegram_broadcast

# NOTE: npx works for local dev but not in NixOS sandboxed services.
# See the NixOS Deployment section for nix-packaged MCP servers.
mcp_servers:
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

The system prompt is assembled from a configurable list of inputs. Each input is either a `workspace_file` (read from `workspace_dir`) or inline `text`. When the `system_prompt` key is omitted, it defaults to `[{type: workspace_file, path: SOUL.md}]`.

Place workspace files in your `workspace_dir` (e.g. `SOUL.md` for personality and behavioral guidelines).

Webhook and cron job prompts use the same input format. Webhook text inputs support `{{.field}}` template placeholders for dynamic content from the JSON payload.

## NixOS Deployment

Add the flake to your NixOS configuration:

```nix
{
  inputs.elwood.url = "github:mcwitt/elwood";
  inputs.mcp-servers.url = "github:nix-community/mcp-servers-nix";

  outputs = { self, nixpkgs, elwood, mcp-servers, ... }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      modules = [
        elwood.nixosModules.default
        ({ pkgs, system, ... }: {
          services.assistant.agents.elwood = {
            enable = true;
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/elwood-env";
            workspaceDir = "/var/lib/assistant/elwood/workspace";

            systemPrompt = [
              {
                type = "workspace_file";
                path = "SOUL.md";
                defaultContent = ''You are Elwood, a personal AI assistant'';
              }
              {
                type = "text";
                content = ''<additional content, not editable by the agent>'';
              }
            ];

            # environmentFile should contain WEBHOOK_SECRET (and TELEGRAM_BOT_TOKEN, ANTHROPIC_API_KEY)
            webhook = {
              enable = true;
              port = 8080;
              endpoints."doorbell" = {
                prompt = [ { type = "text"; content = "Motion detected at {{.timestamp}}"; } ];
                deliveryTargets = [ { type = "telegram_broadcast"; } ];
              };
            };

            # Cron jobs are systemd timers that POST to auto-generated webhook endpoints
            cronJobs.heartbeat = {
              prompt = [ { type = "text"; content = "Check system health. Reply HEARTBEAT_OK if all is well."; } ];
              schedule = "*-*-* *:00/30";  # every 30 minutes
              session = "123456789";       # share conversation with Telegram chat
              deliveryTargets = [ { type = "telegram"; session = "123456789"; } ];
              suppressIfContains = "HEARTBEAT_OK";
            };

            cronJobs.daily-summary = {
              prompt = [ { type = "text"; content = "Generate my daily summary"; } ];
              schedule = "*-*-* 08:00";
              deliveryTargets = [ { type = "telegram_broadcast"; } ];  # broadcast (default)
              # session = null (default); each run is isolated
            };

            mcpServers.filesystem = {
              command = "${mcp-servers.packages.${system}.filesystem}/bin/mcp-server-filesystem";
              args = [ "/var/lib/assistant/elwood/workspace" ];
            };

            permissions = {
              dangerousPatterns = [ "\\brm\\b" "\\bsudo\\b" ];
              safePatterns = [ "^rm -i\\b" ];
              defaultPolicy = "ask";  # Telegram prompts; webhooks deny (use "allow" for webhook-safe tools)
            };
          };

          # Run a second agent with different config
          services.assistant.agents.career-coach = {
            enable = true;
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/career-coach-env";
            model = "claude-sonnet-4-20250514";
          };
        })
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
| `elwood_cost_dollars` | counter | model, source | Approximate cumulative API cost in USD |
| `elwood_tools_registered` | gauge | — | Number of registered tools |
| `elwood_mcp_servers_active` | gauge | — | Number of active MCP servers |
| `elwood_uptime_seconds` | gauge | — | Time since process start |

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
