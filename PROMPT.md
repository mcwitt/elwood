# Build a Self-Hosted Personal AI Assistant

## Project Overview

Build a self-hosted, headless personal AI assistant that runs as a system service on a NixOS server. The assistant communicates via messaging platforms (starting with Telegram), executes tools on the user's behalf, maintains persistent memory across sessions, and proactively wakes on a schedule to surface relevant information.

This is a from-scratch implementation inspired by OpenClaw's architecture, but designed to be minimal, auditable, and tightly integrated with NixOS (systemd service, declarative config, agenix/sops-nix secrets). The user's preferred implementation language is Haskell, but the architecture is language-agnostic.

The working name of the project is "Elwood" (Claude Shannon's middle name).

---

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    System Service                       │
│                  (systemd, NixOS)                       │
│                                                         │
│  ┌──────────┐   ┌────────────┐   ┌──────────────────┐  │
│  │ Channels │──▶│   Agent    │──▶│  Tool Dispatch   │  │
│  │(Telegram)│◀──│   Loop     │◀──│  (built-in +     │  │
│  └──────────┘   │            │   │   MCP + skills)  │  │
│                 │  ┌───────┐ │   └──────────────────┘  │
│  ┌──────────┐   │  │Session│ │   ┌──────────────────┐  │
│  │Scheduler │──▶│  │Store  │ │   │     Memory       │  │
│  │(heartbeat│   │  │(JSONL)│ │   │  (file-based)    │  │
│  │ + cron)  │   │  └───────┘ │   └──────────────────┘  │
│  └──────────┘   └────────────┘                         │
│                                                         │
│  Config: SOUL.md, AGENTS.md, HEARTBEAT.md, tools.yaml  │
│  State:  /var/lib/assistant/                            │
│  Secrets: EnvironmentFile (agenix/sops-nix)             │
└─────────────────────────────────────────────────────────┘
```

The process is a single long-running async application that multiplexes:
1. Inbound messages from channel adapters
2. Scheduled heartbeat/cron triggers
3. (Optionally) a local HTTP API for debugging/scripting

All mutable state lives under a single state directory (e.g. `/var/lib/assistant/`). The process should be stateless on restart — all context is reconstructed from the state directory.

---

## Components to Implement

### 1. Agent Loop (core)

The central reasoning engine. Implements the standard LLM tool-use loop:

**Behavior:**
- Accept a session ID and user message
- Load the session's message history from disk
- Inject the system prompt (concatenation of SOUL.md + AGENTS.md + tool descriptions)
- Call the LLM API with messages + tool schemas
- If the LLM returns tool_use blocks: execute each tool, append results, and loop
- If the LLM returns end_turn: extract text, persist session, return reply
- Acquire a per-session lock before processing to prevent concurrent corruption

**LLM API details:**
- Use the Anthropic Messages API (POST https://api.anthropic.com/v1/messages)
- Model: configurable, default `claude-sonnet-4-5-20250929`
- The API returns `stop_reason: "tool_use"` when the model wants to call tools
- Tool results are sent back as `{"type": "tool_result", "tool_use_id": "...", "content": "..."}`
- The loop continues until `stop_reason: "end_turn"`
- The system prompt is passed via the `system` parameter

**Interface sketch (pseudocode):**
```
agent.runTurn(sessionId: String, userMessage: String) -> IO String
```

### 2. Session Store

Append-only JSONL persistence for conversation history.

**Behavior:**
- Each session is a file: `<stateDir>/sessions/<sessionId>.jsonl`
- Each line is a JSON-encoded message object (`{role, content}`)
- `load(sessionId)` reads all lines into a message list
- `append(sessionId, message)` appends a single JSON line (crash-safe)
- `save(sessionId, messages)` overwrites atomically (write-to-temp + rename)

**Context compaction:**
- Before each agent turn, estimate token count (rough heuristic: `sum(len(json(msg))) / 4`)
- If above threshold (e.g. 100k tokens, ~80% of context window), compact:
  - Split messages in half (old / recent)
  - Summarize old half with a cheap/fast model call (e.g. Haiku)
  - Replace old half with a single synthetic message containing the summary
  - Persist the compacted session

### 3. Telegram Channel Adapter

Thin adapter that bridges Telegram Bot API to the agent loop.

**Behavior:**
- Long-poll for updates using the Telegram Bot API (or use a client library)
- On receiving a text message:
  - Check sender against an allowlist (chat IDs from config/env)
  - Derive session ID from sender's user ID (or chat ID for groups)
  - Call `agent.runTurn(sessionId, messageText)`
  - Send the reply back via `sendMessage`
- Support an outbound `notify(chatId, text)` method for proactive messages (used by heartbeat)
- Handle long replies by splitting at message length limits (4096 chars for Telegram)

**Allowlist / pairing (optional but recommended):**
- Unknown senders receive a short pairing code
- The code is approved via a CLI command or config file
- Until approved, no messages are processed

### 4. System Prompt Assembly

Load and concatenate workspace documents into the system prompt.

**Behavior:**
- Read these files from the workspace directory (configurable path):
  - `SOUL.md` — personality, tone, behavioral boundaries
  - `AGENTS.md` — operational instructions, tool-use guidelines
  - `TOOLS.md` — additional tool/skill descriptions (optional)
- Concatenate with clear section separators
- Append auto-generated tool schema descriptions (if not handled by the API's native tool parameter)
- Optionally support live-reloading (re-read on each turn, or watch for file changes)

### 5. Built-in Tools

Implement these core tools that the agent can invoke:

#### a) `run_command`
- Execute a shell command via subprocess
- Enforce a timeout (default 30s)
- Return stdout + stderr
- **Safety:** Check against an allowlist/denylist before execution (see §9)

#### b) `read_file` / `write_file`
- Read or write files within a scoped directory (the workspace or a designated scratch area)
- Reject paths outside the allowed scope (path traversal protection)

#### c) `web_search`
- Call an external search API (Brave Search, SearXNG, or similar)
- Return a formatted summary of top results
- Requires an API key (from environment)

#### d) `web_fetch`
- HTTP GET a URL, extract text content (strip HTML)
- Enforce a size limit and timeout
- Useful for reading articles, docs, API responses

#### e) `save_memory` / `search_memory`
- See §7 (Memory Store)

Each tool is defined by:
1. A **schema** (name, description, input JSON schema) — passed to the LLM API in the `tools` parameter
2. An **executor** function — called when the LLM invokes the tool

### 6. MCP Client

Integrate with external MCP (Model Context Protocol) servers for extensibility.

**Behavior:**
- MCP servers are external processes that expose tools over JSON-RPC 2.0
- Two transport modes: **stdio** (spawn a subprocess, communicate via stdin/stdout) and **HTTP-SSE**
- On startup, read MCP server configs from a config file (list of `{name, command, args, env}`)
- For each configured server:
  - Spawn the process (stdio) or connect (HTTP)
  - Call `tools/list` to discover available tools
  - Merge discovered tool schemas into the agent's tool list
- When the agent invokes an MCP tool, route to the appropriate server via `tools/call`

**Lifecycle:**
- MCP servers should be started on demand or at service startup
- Handle server crashes gracefully (retry, remove from tool list)
- Optionally run MCP servers as separate systemd services on NixOS

**Interface sketch:**
```
mcpClient.listTools(serverName) -> [ToolSchema]
mcpClient.callTool(serverName, toolName, arguments) -> ToolResult
```

### 7. Memory Store

Persistent, cross-session knowledge store.

**Behavior:**
- Directory of markdown files: `<stateDir>/memory/<key>.md`
- `save_memory(key, content)` — write/overwrite a memory file
- `search_memory(query)` — keyword search across all memory files
  - Split query into words, check for substring matches in file contents
  - Return matching files with their contents
- Expose both as tools in the agent's tool schema

**Future upgrade path:**
- Add a vector index (e.g. SQLite with vector extension, or ChromaDB) for semantic search
- The tool interface stays the same — only the search implementation changes

### 8. Scheduler (Heartbeat + Cron)

Proactive agent execution on timers.

#### Heartbeat
- A recurring timer (default every 30 minutes, configurable)
- On each tick:
  - Check if current time is within active hours (e.g. 08:00–22:00)
  - If outside active hours, skip
  - Read `HEARTBEAT.md` from workspace (a checklist of things to monitor)
  - Inject it as a user message into a dedicated "heartbeat" session:
    `"Heartbeat check. Follow this checklist:\n{heartbeat_md}\nIf nothing needs attention, reply HEARTBEAT_OK."`
  - Run `agent.runTurn("heartbeat", prompt)`
  - If the reply contains "HEARTBEAT_OK", suppress (don't send to user)
  - Otherwise, send the reply to the user via the Telegram channel's `notify` method

#### Cron Jobs
- A list of scheduled jobs defined in config (name, cron expression, message, isolated session flag)
- On each tick, evaluate which jobs are due
- For each due job:
  - If `isolated: true`, run in a fresh session (no shared history)
  - If `isolated: false`, run in the main/heartbeat session
  - Send the configured message as the user prompt
  - Deliver the reply to the user

**Implementation options:**
- Use an in-process cron library / scheduler
- Or use NixOS systemd timers for cron jobs (invoke via the HTTP API or a CLI subcommand)

### 9. Permission / Safety Layer

Control what the agent is allowed to do.

**Behavior:**
- Maintain a tool allowlist in config: `{toolName: "allow" | "ask" | "deny"}`
- For `run_command` specifically:
  - A safe-command set (e.g. `ls`, `cat`, `date`, `git status`) → auto-allow
  - A dangerous-pattern regex list (e.g. `rm`, `sudo`, `curl|sh`) → block or require approval
  - A persistent approvals file (`<stateDir>/approvals.json`) for previously approved commands
- For tools marked `"ask"`:
  - Send an approval request to the user via Telegram (inline keyboard: Approve / Deny)
  - Await the callback response before proceeding
  - Persist the approval for future use
- This is the application-level layer; the NixOS systemd hardening (below) provides the OS-level layer

---

## NixOS Integration

### Flake Structure

```
flake.nix                  # inputs: nixpkgs, (optionally nix-openclaw for MCP tools)
├── modules/
│   └── assistant.nix      # NixOS module
├── src/                   # application source code
├── workspace/
│   ├── SOUL.md
│   ├── AGENTS.md
│   ├── HEARTBEAT.md
│   └── skills/
└── package.nix            # or default.nix — builds the application
```

### NixOS Module (`assistant.nix`)

The module should expose these options under `services.assistant`:

```nix
services.assistant = {
  enable = mkEnableOption "personal AI assistant";
  
  # Path to the built application binary/entrypoint
  package = mkOption { type = types.package; };
  
  # Workspace directory containing SOUL.md, AGENTS.md, etc.
  # These are copied/symlinked into the state directory on activation
  workspaceDir = mkOption { type = types.path; };
  
  # State directory (default: /var/lib/assistant)
  stateDir = mkOption { type = types.str; default = "/var/lib/assistant"; };
  
  # Path to EnvironmentFile containing secrets:
  #   TELEGRAM_BOT_TOKEN=...
  #   ANTHROPIC_API_KEY=...
  #   BRAVE_SEARCH_API_KEY=...  (optional)
  #   GATEWAY_AUTH_TOKEN=...     (optional, for HTTP API)
  environmentFile = mkOption { type = types.path; };
  
  # Telegram chat IDs allowed to interact
  allowedChatIds = mkOption { type = types.listOf types.str; default = []; };
  
  # Model configuration
  model = mkOption { type = types.str; default = "claude-sonnet-4-5-20250929"; };
  
  # Heartbeat interval in minutes (0 to disable)
  heartbeatInterval = mkOption { type = types.int; default = 30; };
  
  # Active hours for heartbeat (24h format)
  activeHoursStart = mkOption { type = types.int; default = 8; };
  activeHoursEnd = mkOption { type = types.int; default = 22; };
  
  # MCP server configurations
  mcpServers = mkOption {
    type = types.attrsOf (types.submodule { ... });
    default = {};
  };
};
```

### systemd Service

```nix
systemd.services.assistant = {
  description = "Personal AI Assistant";
  after = [ "network-online.target" ];
  wants = [ "network-online.target" ];
  wantedBy = [ "multi-user.target" ];
  
  serviceConfig = {
    Type = "exec";
    ExecStart = "${cfg.package}/bin/assistant";
    Restart = "always";
    RestartSec = 10;
    
    # State
    StateDirectory = "assistant";
    WorkingDirectory = cfg.stateDir;
    
    # Secrets
    EnvironmentFile = cfg.environmentFile;
    
    # Hardening
    DynamicUser = false;  # need stable UID for state dir ownership
    User = "assistant";
    Group = "assistant";
    NoNewPrivileges = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    PrivateTmp = true;
    PrivateDevices = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectControlGroups = true;
    RestrictNamespaces = true;
    RestrictSUIDSGID = true;
    MemoryDenyWriteExecute = true;
    ReadWritePaths = [ cfg.stateDir ];
    
    # Capability restrictions
    CapabilityBoundingSet = "";
    AmbientCapabilities = "";
    
    # System call filter (allow basic syscalls + networking)
    SystemCallFilter = [ "@system-service" "~@privileged" "~@resources" ];
    SystemCallArchitectures = "native";
  };
};

# Create the service user
users.users.assistant = {
  isSystemUser = true;
  group = "assistant";
  home = cfg.stateDir;
};
users.groups.assistant = {};
```

### Activation Script

Symlink workspace files into the state directory on `nixos-rebuild switch`:

```nix
system.activationScripts.assistant-workspace = lib.stringAfter [ "users" ] ''
  mkdir -p ${cfg.stateDir}/workspace
  mkdir -p ${cfg.stateDir}/sessions
  mkdir -p ${cfg.stateDir}/memory
  
  # Symlink workspace documents
  for f in ${cfg.workspaceDir}/*; do
    ln -sf "$f" ${cfg.stateDir}/workspace/$(basename "$f")
  done
  
  chown -R assistant:assistant ${cfg.stateDir}
'';
```

---

## Configuration

The application reads its configuration from a combination of:
1. **Environment variables** — secrets (API keys, tokens)
2. **A config file** — non-secret settings (model, heartbeat interval, MCP servers, allowed chat IDs)
3. **Workspace files** — SOUL.md, AGENTS.md, HEARTBEAT.md, skills/

The config file format can be YAML, TOML, or JSON. It should be generated by the NixOS module from the declared options.

Example config:

```yaml
model: "claude-sonnet-4-5-20250929"
compaction_model: "claude-haiku-4-5-20251001"
state_dir: "/var/lib/assistant"
workspace_dir: "/var/lib/assistant/workspace"

telegram:
  allowed_chat_ids: ["123456789"]

heartbeat:
  interval_minutes: 30
  active_hours_start: 8
  active_hours_end: 22

permissions:
  safe_commands: ["ls", "cat", "head", "tail", "date", "whoami", "echo", "git"]
  dangerous_patterns: ["\\brm\\b", "\\bsudo\\b", "\\bchmod\\b", "curl.*\\|.*sh"]
  tool_policy:
    run_command: "ask"
    read_file: "allow"
    write_file: "allow"
    web_search: "allow"
    web_fetch: "allow"
    save_memory: "allow"
    search_memory: "allow"

mcp_servers:
  filesystem:
    command: "npx"
    args: ["-y", "@modelcontextprotocol/server-filesystem", "/home/user/docs"]
```

---

## Implementation Notes

### Error Handling
- Network errors to the Anthropic API should be retried with exponential backoff
- Tool execution errors should be returned to the LLM as error-typed tool results (the model can often recover)
- Telegram polling errors should be logged and retried
- MCP server crashes should be logged; the server should be removed from the active tool list and restarted

### Logging
- Structured logging to stdout/stderr (systemd journal captures it)
- Log levels: ERROR (failures), WARN (retries, permission denials), INFO (turns, tool calls), DEBUG (full payloads)
- Never log secrets or full API keys

### Testing
- The agent loop should be testable with a mock LLM client (return canned tool_use / end_turn responses)
- Session store should be testable with a temp directory
- Channel adapters should be testable with a mock send/receive interface

### Concurrency
- One agent turn per session at a time (per-session lock)
- Different sessions can run concurrently
- Telegram message handler and scheduler run concurrently
- Use async I/O throughout (important for responsiveness during tool execution)

---

## Milestone Plan

### M1: Skeleton + Echo Bot
- Project scaffolding (build system, directory structure, dependencies)
- Config loading (read YAML/TOML + environment)
- Telegram adapter: connect, receive messages, echo them back
- NixOS module: service definition, user creation, environment file
- **Deliverable:** `nixos-rebuild switch` starts a Telegram echo bot

### M2: Agent Loop + Sessions
- Anthropic API client (messages endpoint with tool_use support)
- Session store (JSONL load/append/save)
- Agent loop (the tool_use → execute → loop cycle)
- System prompt assembly (SOUL.md loading)
- Wire Telegram adapter → agent loop
- **Deliverable:** Can have a multi-turn conversation via Telegram, persisted across restarts

### M3: Built-in Tools
- Implement tool schemas + executors for: run_command, read_file, write_file, web_search, web_fetch
- Permission layer (allowlist, safe commands, dangerous patterns)
- Wire tools into agent loop
- **Deliverable:** "What files are in my workspace?" / "Search the web for X" work via Telegram

### M4: Memory + Compaction
- Memory store (file-based save + keyword search)
- save_memory / search_memory tools
- Context compaction (detect oversize sessions, summarize old half)
- **Deliverable:** Agent remembers facts across session resets, long conversations don't break

### M5: Heartbeat + Scheduling
- Heartbeat timer with active hours
- HEARTBEAT.md loading and injection
- HEARTBEAT_OK suppression
- Proactive notification via Telegram
- Basic cron job support (config-driven, isolated sessions)
- **Deliverable:** Agent wakes every 30 min, checks heartbeat list, notifies only when relevant

### M6: MCP Integration
- MCP stdio client (spawn process, JSON-RPC over stdin/stdout)
- Tool discovery (tools/list) and routing (tools/call)
- Config-driven MCP server management
- **Deliverable:** Can add an MCP server to config and the agent discovers + uses its tools

### M7: Polish
- Approval flow via Telegram inline keyboards (for "ask" tools)
- Pairing flow for unknown senders
- HTTP API for debugging (list sessions, trigger heartbeat, view logs)
- Context compaction refinements (better token estimation, configurable thresholds)
- Comprehensive error handling and retry logic
