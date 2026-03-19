{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.assistant;

  # Pruning/compaction strategy: {keepTurns = N;} or {keepFraction = F;}.
  # Tags are camelCase per NixOS convention; strategyToYaml converts to
  # snake_case keys (keep_turns/keep_fraction) for the Haskell YAML parser.
  strategyType = lib.types.attrTag {
    keepTurns = lib.mkOption {
      type = lib.types.int;
      description = "Number of recent turns to protect.";
    };
    keepFraction = lib.mkOption {
      type = lib.types.float;
      description = "Fraction of total turns to protect.";
    };
  };

  # Convert camelCase strategy tag to snake_case YAML key.
  strategyToYaml =
    s:
    if s ? keepTurns then
      { keep_turns = s.keepTurns; }
    else if s ? keepFraction then
      { keep_fraction = s.keepFraction; }
    else
      throw "strategyToYaml: unexpected strategy tag: ${builtins.toJSON s}";

  # Thinking mode: {adaptive = {effort = "medium";};} or {fixed = {budgetTokens = N;};}.
  # Tags are camelCase per NixOS convention; mkThinkingModeYaml converts to
  # snake_case keys (budget_tokens) for the Haskell YAML parser.
  thinkingModeType = lib.types.attrTag {
    adaptive = lib.mkOption {
      type = lib.types.submodule {
        options.effort = lib.mkOption {
          type = lib.types.nullOr (
            lib.types.enum [
              "low"
              "medium"
              "high"
            ]
          );
          default = null;
          description = "Effort level for adaptive thinking. Null uses API default.";
          example = "medium";
        };
      };
      description = "Adaptive thinking with optional effort level.";
    };
    fixed = lib.mkOption {
      type = lib.types.submodule {
        options.budgetTokens = lib.mkOption {
          type = lib.types.int;
          description = "Fixed token budget for thinking.";
        };
      };
      description = "Fixed thinking budget.";
    };
  };

  # Generate YAML config for an agent
  mkConfigFile =
    name: agentCfg:
    let
      # Build webhook endpoints (includes auto-generated cron endpoints)
      # Serialize a deliver target submodule to a YAML-ready attrset
      mkDeliveryTarget =
        dt:
        {
          type = dt.type;
        }
        // lib.optionalAttrs (dt.type == "telegram") {
          chat_ids = dt.chatIds;
        };

      # Serialize a prompt input to a YAML-ready attrset
      mkPromptInputYaml =
        pi:
        {
          inherit (pi) type;
        }
        // lib.optionalAttrs (pi.path != null) {
          inherit (pi) path;
        }
        // lib.optionalAttrs (pi.content != null) {
          inherit (pi) content;
        };

      # Serialize permission overrides to a YAML-ready attrset (only non-null fields)
      mkPermissionOverridesAttrs =
        perms:
        { }
        // lib.optionalAttrs (perms.safePatterns != null) {
          safe_patterns = perms.safePatterns;
        }
        // lib.optionalAttrs (perms.dangerousPatterns != null) {
          dangerous_patterns = perms.dangerousPatterns;
        }
        // lib.optionalAttrs (perms.toolPolicies != null) {
          tool_policies = perms.toolPolicies;
        }
        // lib.optionalAttrs (perms.defaultPolicy != null) {
          default_policy = perms.defaultPolicy;
        }
        // lib.optionalAttrs (perms.approvalTimeoutSeconds != null) {
          approval_timeout_seconds = perms.approvalTimeoutSeconds;
        };

      # Serialize agent overrides to a flat attrset of YAML keys (no wrapper)
      mkAgentOverridesAttrs =
        agentOvr:
        { }
        // lib.optionalAttrs (agentOvr.model != null) { model = agentOvr.model; }
        // (
          let
            thinkingAttrs = mkThinkingOverridesAttrs agentOvr.thinking;
          in
          lib.optionalAttrs (thinkingAttrs != { }) { thinking = thinkingAttrs; }
        )
        // lib.optionalAttrs (agentOvr.maxIterations != null) {
          max_iterations = agentOvr.maxIterations;
        }
        // (
          let
            cacheAttrs =
              { }
              // lib.optionalAttrs (agentOvr.cache.enable != null) { enable = agentOvr.cache.enable; }
              // lib.optionalAttrs (agentOvr.cache.ttl != null) { ttl = agentOvr.cache.ttl; };
          in
          lib.optionalAttrs (cacheAttrs != { }) { cache = cacheAttrs; }
        )
        // lib.optionalAttrs (agentOvr.maxTokens != null) {
          max_tokens = agentOvr.maxTokens;
        }
        // lib.optionalAttrs (agentOvr.systemPrompt != null) {
          system_prompt = map mkPromptInputYaml agentOvr.systemPrompt;
        }
        // lib.optionalAttrs (agentOvr.toolSearch != null) {
          tool_search = agentOvr.toolSearch;
        }
        // (
          let
            permAttrs =
              if agentOvr.permissions != null then mkPermissionOverridesAttrs agentOvr.permissions else { };
          in
          lib.optionalAttrs (permAttrs != { }) { permissions = permAttrs; }
        );

      # Serialize agent preset (overrides + optional description)
      mkAgentPresetAttrs =
        preset:
        mkAgentOverridesAttrs preset
        // lib.optionalAttrs (preset.description != null) {
          description = preset.description;
        };

      # Serialize agent overrides to a nested `agent` attrset (omitted when empty)
      mkAgentOverrides =
        agentOvr:
        let
          attrs = mkAgentOverridesAttrs agentOvr;
        in
        lib.optionalAttrs (attrs != { }) { agent = attrs; };

      # Serialize common endpoint fields to a YAML-ready attrset
      mkEndpointYaml =
        epName: epCfg:
        {
          name = epName;
          prompt = map mkPromptInputYaml epCfg.prompt;
          session = epCfg.session;
          delivery_target = mkDeliveryTarget epCfg.deliveryTarget;
        }
        // lib.optionalAttrs (epCfg.suppressIfContains != null) {
          suppress_if_contains = epCfg.suppressIfContains;
        }
        // mkAgentOverrides epCfg.agent;

      allWebhookEndpoints =
        let
          # User-defined endpoints
          userEndpoints = lib.mapAttrsToList (
            epName: epCfg: mkEndpointYaml epName epCfg
          ) agentCfg.webhook.endpoints;

          # Auto-generated cron job endpoints
          cronEndpoints = lib.mapAttrsToList (
            cronName: cronCfg: mkEndpointYaml "cron-${cronName}" cronCfg
          ) agentCfg.cronJobs;
        in
        userEndpoints ++ cronEndpoints;

      # Build MCP servers
      mcpServersList = lib.mapAttrs (
        serverName: serverCfg:
        {
          command = serverCfg.command;
          args = serverCfg.args;
        }
        // lib.optionalAttrs (serverCfg.env != { }) {
          env = serverCfg.env;
        }
        // lib.optionalAttrs (serverCfg.startupDelay != 0) {
          startup_delay = serverCfg.startupDelay;
        }
      ) agentCfg.mcpServers;

      configContent = {
        state_dir = agentCfg.stateDir;
        workspace = agentCfg.workspace.path;
        channels.telegram = map (
          tc:
          {
            inherit (tc) id;
          }
          // lib.optionalAttrs (tc.session != null) {
            inherit (tc) session;
          }
          // mkAgentOverrides tc.agent
        ) agentCfg.channels.telegram;
        agent = {
          model = agentCfg.agent.model;
          thinking = mkThinkingConfig agentCfg.agent.thinking;
          max_iterations = agentCfg.agent.maxIterations;
          cache = {
            enable = agentCfg.agent.cache.enable;
            ttl = agentCfg.agent.cache.ttl;
          };
          max_tokens = agentCfg.agent.maxTokens;
          system_prompt = map mkPromptInputYaml agentCfg.agent.systemPrompt;
          permissions = {
            safe_patterns = agentCfg.agent.permissions.safePatterns;
            dangerous_patterns = agentCfg.agent.permissions.dangerousPatterns;
            tool_policies = agentCfg.agent.permissions.toolPolicies;
            default_policy = agentCfg.agent.permissions.defaultPolicy;
            approval_timeout_seconds = agentCfg.agent.permissions.approvalTimeoutSeconds;
          };
        }
        // lib.optionalAttrs (agentCfg.agent.toolSearch != null) {
          tool_search = agentCfg.agent.toolSearch;
        };
        max_image_dimension = agentCfg.maxImageDimension;
        tool_use_messages = agentCfg.toolUseMessages;

        compaction = {
          enable = agentCfg.compaction.enable;
          token_threshold = agentCfg.compaction.tokenThreshold;
          model = agentCfg.compaction.model;
          strategy = strategyToYaml agentCfg.compaction.strategy;
        }
        // lib.optionalAttrs (agentCfg.compaction.prompt != null) {
          prompt = agentCfg.compaction.prompt;
        };

        pruning = {
          enable = agentCfg.pruning.enable;
          strategy = strategyToYaml agentCfg.pruning.strategy;
        }
        // (
          if agentCfg.pruning.thinking.enable then
            {
              thinking =
                { }
                // lib.optionalAttrs (agentCfg.pruning.thinking.strategy != null) {
                  strategy = strategyToYaml agentCfg.pruning.thinking.strategy;
                };
            }
          else
            { thinking = null; }
        )
        // {
          tools = {
            enable = agentCfg.pruning.tools.enable;
            head_chars = agentCfg.pruning.tools.headChars;
            tail_chars = agentCfg.pruning.tools.tailChars;
          }
          // lib.optionalAttrs (agentCfg.pruning.tools.strategy != null) {
            strategy = strategyToYaml agentCfg.pruning.tools.strategy;
          }
          //
            lib.optionalAttrs
              (
                agentCfg.pruning.tools.input.strategy != null
                || agentCfg.pruning.tools.input.headChars != null
                || agentCfg.pruning.tools.input.tailChars != null
              )
              {
                input =
                  { }
                  // lib.optionalAttrs (agentCfg.pruning.tools.input.strategy != null) {
                    strategy = strategyToYaml agentCfg.pruning.tools.input.strategy;
                  }
                  // lib.optionalAttrs (agentCfg.pruning.tools.input.headChars != null) {
                    head_chars = agentCfg.pruning.tools.input.headChars;
                  }
                  // lib.optionalAttrs (agentCfg.pruning.tools.input.tailChars != null) {
                    tail_chars = agentCfg.pruning.tools.input.tailChars;
                  };
              }
          //
            lib.optionalAttrs
              (
                agentCfg.pruning.tools.output.strategy != null
                || agentCfg.pruning.tools.output.headChars != null
                || agentCfg.pruning.tools.output.tailChars != null
              )
              {
                output =
                  { }
                  // lib.optionalAttrs (agentCfg.pruning.tools.output.strategy != null) {
                    strategy = strategyToYaml agentCfg.pruning.tools.output.strategy;
                  }
                  // lib.optionalAttrs (agentCfg.pruning.tools.output.headChars != null) {
                    head_chars = agentCfg.pruning.tools.output.headChars;
                  }
                  // lib.optionalAttrs (agentCfg.pruning.tools.output.tailChars != null) {
                    tail_chars = agentCfg.pruning.tools.output.tailChars;
                  };
              };
        };
      }
      // lib.optionalAttrs (mcpServersList != { }) {
        mcp_servers = mcpServersList;
      }
      // lib.optionalAttrs agentCfg.webhook.enable {
        webhook = {
          enabled = true;
          port = agentCfg.webhook.port;
        }
        // lib.optionalAttrs (allWebhookEndpoints != [ ]) {
          endpoints = allWebhookEndpoints;
        };
      }
      // (
        let
          delegateAgentAttrs = mkAgentPresetAttrs agentCfg.delegate.agent;
          extraAgentsAttrs = lib.mapAttrs (_: mkAgentPresetAttrs) agentCfg.delegate.extraAgents;
          delegateAttrs =
            lib.optionalAttrs (delegateAgentAttrs != { }) {
              agent = delegateAgentAttrs;
            }
            // lib.optionalAttrs (extraAgentsAttrs != { }) {
              extra_agents = extraAgentsAttrs;
            }
            // lib.optionalAttrs (agentCfg.delegate.allowedModels != null) {
              allowed_models = agentCfg.delegate.allowedModels;
            };
        in
        lib.optionalAttrs (delegateAttrs != { }) {
          delegate = delegateAttrs;
        }
      );
    in
    pkgs.writeText "assistant-${name}-config.yaml" (lib.generators.toYAML { } configContent);

  # Generate ExecStartPre script for provisioning workspace files.
  # Immutable files (default) are written unconditionally on every start.
  # Mutable files are only written if they don't already exist.
  mkWorkspaceFilesScript =
    agentCfg:
    let
      files = agentCfg.workspace.files;
      mkFileEntry =
        name:
        let
          fileCfg = files.${name};
          fullPath = "${agentCfg.workspace.path}/${name}";
          # Content lives in the Nix store; the script copies it into place.
          storePath = if fileCfg.text != null then pkgs.writeText name fileCfg.text else fileCfg.path;
          copyCmd = "install -m 0640 ${storePath} ${lib.escapeShellArg fullPath}";
        in
        ''
          mkdir -p "$(dirname ${lib.escapeShellArg fullPath})"
        ''
        + (
          if fileCfg.mutable then
            ''
              if [ ! -f ${lib.escapeShellArg fullPath} ]; then
                ${copyCmd}
              fi
            ''
          else
            "  ${copyCmd}\n"
        );
    in
    if files == { } then
      null
    else
      pkgs.writeShellScript "assistant-workspace-files" (
        lib.concatMapStrings mkFileEntry (lib.attrNames files)
      );

  # Serialize a thinking mode to YAML, converting camelCase to snake_case keys.
  mkThinkingModeYaml =
    mode:
    if mode ? adaptive then
      {
        adaptive =
          { }
          // lib.optionalAttrs (mode.adaptive.effort != null) {
            effort = mode.adaptive.effort;
          };
      }
    else if mode ? fixed then
      {
        fixed = {
          budget_tokens = mode.fixed.budgetTokens;
        };
      }
    else
      throw "mkThinkingModeYaml: unexpected thinking mode tag: ${builtins.toJSON mode}";

  # Serialize thinking config (enable + mode) to a YAML-ready attrset.
  mkThinkingConfig = tc: {
    enable = tc.enable;
    mode = mkThinkingModeYaml tc.mode;
  };

  # Serialize thinking overrides (nullable fields) to a YAML-ready attrset.
  mkThinkingOverridesAttrs =
    tc:
    { }
    // lib.optionalAttrs (tc.enable != null) { enable = tc.enable; }
    // lib.optionalAttrs (tc.mode != null) { mode = mkThinkingModeYaml tc.mode; };

  # Submodule for delivery targets
  deliveryTargetModule = lib.types.submodule {
    options = {
      type = lib.mkOption {
        type = lib.types.enum [
          "telegram"
          "telegram_broadcast"
          "log_only"
        ];
        description = "Delivery target type.";
      };

      chatIds = lib.mkOption {
        type = lib.types.listOf lib.types.int;
        default = [ ];
        description = "Telegram chat IDs to send to (telegram type only).";
        example = [ 123456789 ];
      };
    };
  };

  # Submodule for prompt inputs
  promptInputModule = lib.types.submodule {
    options = {
      type = lib.mkOption {
        type = lib.types.enum [
          "workspace_file"
          "text"
        ];
        description = "Prompt input type: workspace_file reads from workspace directory, text is inline content.";
      };

      path = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "File path relative to workspace directory (workspaceFile type only).";
      };

      content = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Inline text content (text type only).";
      };

    };
  };

  # Submodule for permission overrides (all fields nullable for selective override)
  permissionOverrideModule = lib.types.submodule {
    options = {
      safePatterns = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        default = null;
        description = "Regex patterns that override dangerousPatterns (always allowed).";
      };

      dangerousPatterns = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        default = null;
        description = "Regex patterns that are always blocked.";
      };

      toolPolicies = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.attrsOf (
            lib.types.enum [
              "allow"
              "ask"
              "deny"
            ]
          )
        );
        default = null;
        description = "Per-tool policies (allow, ask, deny).";
      };

      defaultPolicy = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "allow"
            "ask"
            "deny"
          ]
        );
        default = null;
        description = "Default policy for tools not in toolPolicies.";
      };

      approvalTimeoutSeconds = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = "Timeout for approval requests.";
      };
    };
  };

  # Agent override options shared between per-chat, endpoint, and cron contexts
  agentOverrideOptions = {
    model = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Model override. Null means use the agent's global model.";
      example = "claude-haiku-4-20250414";
    };

    thinking = {
      enable = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = "Thinking enable override. Null means use the agent's global value.";
      };
      mode = lib.mkOption {
        type = lib.types.nullOr thinkingModeType;
        default = null;
        description = "Thinking mode override. Null means use the agent's global mode.";
      };
    };

    maxIterations = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Max iterations override. Null means use the agent's global value.";
    };

    cache = {
      enable = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = "Cache enable override. Null means use the agent's global value.";
      };
      ttl = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "5m"
            "1h"
          ]
        );
        default = null;
        description = "Cache TTL override. Null means use the agent's global value.";
      };
    };

    maxTokens = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Max tokens override for API responses. Null means use the agent's global value.";
    };

    systemPrompt = lib.mkOption {
      type = lib.types.nullOr (lib.types.listOf promptInputModule);
      default = null;
      description = "System prompt override. Null means inherit from parent.";
    };

    toolSearch = lib.mkOption {
      type = lib.types.nullOr (lib.types.listOf lib.types.str);
      default = null;
      description = "Tool search override. Null means inherit from parent. Empty list enables tool search with all tools deferred.";
    };

    permissions = lib.mkOption {
      type = lib.types.nullOr permissionOverrideModule;
      default = null;
      description = "Permission overrides. Null means inherit from parent. Individual fields are merged, not replaced.";
    };
  };

  # Options shared between webhook endpoints and cron jobs
  commonEndpointOptions = {
    prompt = lib.mkOption {
      type = lib.types.listOf promptInputModule;
      description = "Prompt inputs for this endpoint.";
    };

    session = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Session name for persistent conversation. Null means isolated (no history).";
    };

    deliveryTarget = lib.mkOption {
      type = deliveryTargetModule;
      default = {
        type = "telegram_broadcast";
      };
      description = "Delivery target.";
    };

    suppressIfContains = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Suppress notification if response contains this string.";
      example = "HEARTBEAT_OK";
    };

    agent = agentOverrideOptions;
  };

  # Submodule for webhook endpoints
  webhookEndpointModule = lib.types.submodule {
    options = commonEndpointOptions;
  };

  # Submodule for cron jobs (always use systemd timers)
  cronJobModule = lib.types.submodule {
    options = commonEndpointOptions // {
      schedule = lib.mkOption {
        type = lib.types.either lib.types.str (lib.types.listOf lib.types.str);
        default = "hourly";
        description = ''
          systemd OnCalendar schedule. A single string or a list of strings.
          Use a list for irregular times that can't be expressed as one expression
          (e.g. [ "*-*-* 07:00" "*-*-* 11:30" ] instead of the invalid "*-*-* 07:00,11:30").
        '';
        example = "*-*-* 09:00";
      };
    };
  };

  # Submodule for MCP servers
  mcpServerModule = lib.types.submodule {
    options = {
      command = lib.mkOption {
        type = lib.types.str;
        description = "Command to run the MCP server.";
      };

      args = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Arguments for the MCP server command.";
      };

      env = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
        description = "Environment variables for the MCP server.";
      };

      startupDelay = lib.mkOption {
        type = lib.types.int;
        default = 0;
        description = "Milliseconds to wait after starting before sending initialize.";
      };
    };
  };

  # Submodule for telegram chat configuration
  telegramChatModule = lib.types.submodule {
    options = {
      id = lib.mkOption {
        type = lib.types.int;
        description = "Telegram chat ID.";
      };

      session = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Session name for persistent conversation. Null means isolated (no history).";
      };

      agent = agentOverrideOptions;
    };
  };

  # Submodule for each agent
  agentModule = lib.types.submodule (
    { name, ... }:
    {
      options = {
        enable = lib.mkEnableOption "this assistant agent";

        stateDir = lib.mkOption {
          type = lib.types.path;
          default = "/var/lib/assistant/${name}";
          description = "Directory for persistent state.";
        };

        workspace = {
          path = lib.mkOption {
            type = lib.types.path;
            default = "/var/lib/assistant/${name}/workspace";
            description = "Directory containing SOUL.md and other workspace files.";
          };

          files = lib.mkOption {
            type = lib.types.attrsOf (
              lib.types.submodule {
                options = {
                  text = lib.mkOption {
                    type = lib.types.nullOr lib.types.str;
                    default = null;
                    description = "Inline text content for the file.";
                  };

                  path = lib.mkOption {
                    type = lib.types.nullOr lib.types.path;
                    default = null;
                    description = "Path to a file whose contents are copied into the workspace.";
                  };

                  mutable = lib.mkOption {
                    type = lib.types.bool;
                    default = false;
                    description = "If true, the file is only written when missing (agent can modify it). If false (default), the file is overwritten on every service start.";
                  };
                };
              }
            );
            default = { };
            description = "Files to provision in the workspace directory. Each key is a filename relative to workspace.path.";
            example = lib.literalExpression ''
              {
                "SOUL.md".text = "You are a helpful assistant.";
                "USER.md" = {
                  path = ./USER.md;
                  mutable = true;
                };
              }
            '';
          };
        };

        environmentFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = ''
            Path to environment file with secrets (TELEGRAM_BOT_TOKEN, ANTHROPIC_API_KEY).
          '';
        };

        extraPackages = lib.mkOption {
          type = lib.types.listOf lib.types.package;
          default = [ ];
          description = "Extra packages to add to PATH for run_command tool.";
          example = lib.literalExpression "[ pkgs.python3 pkgs.jq ]";
        };

        channels.telegram = lib.mkOption {
          type = lib.types.listOf telegramChatModule;
          default = [ ];
          description = "Telegram chat configurations (ID + optional session name).";
          example = [
            {
              id = 123456789;
              session = "main";
            }
          ];
        };

        agent = {
          model = lib.mkOption {
            type = lib.types.str;
            default = "claude-sonnet-4-20250514";
            description = "Claude model to use.";
          };

          thinking = {
            enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Enable extended thinking. Default is false.";
            };
            mode = lib.mkOption {
              type = thinkingModeType;
              default = {
                adaptive = { };
              };
              description = "Thinking mode: {adaptive = {};} or {fixed = {budgetTokens = N;};}. Used when enable is true.";
            };
          };

          maxIterations = lib.mkOption {
            type = lib.types.int;
            default = 20;
            description = "Maximum agent loop iterations per turn (prevents infinite tool-use loops).";
          };

          cache = {
            enable = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Enable prompt caching.";
            };
            ttl = lib.mkOption {
              type = lib.types.enum [
                "5m"
                "1h"
              ];
              default = "5m";
              description = ''
                Prompt cache TTL. "5m" for 5-minute ephemeral cache (1.25x write cost),
                "1h" for 1-hour extended cache (2x write cost).
              '';
            };
          };

          maxTokens = lib.mkOption {
            type = lib.types.int;
            default = 16384;
            description = "Maximum tokens for API responses. Since billing is per actual token, a high limit costs nothing extra.";
          };

          systemPrompt = lib.mkOption {
            type = lib.types.listOf promptInputModule;
            default = [
              {
                type = "workspace_file";
                path = "SOUL.md";
              }
            ];
            description = "System prompt inputs. Assembled in order to form the system prompt.";
          };

          toolSearch = lib.mkOption {
            type = lib.types.nullOr (lib.types.listOf lib.types.str);
            default = null;
            description = "Tool names that are never deferred (always available). Null disables tool search.";
            example = [ "run_command" ];
          };

          permissions = {
            safePatterns = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Regex patterns that override dangerousPatterns (always allowed).";
            };

            dangerousPatterns = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Regex patterns that are always blocked.";
            };

            toolPolicies = lib.mkOption {
              type = lib.types.attrsOf (
                lib.types.enum [
                  "allow"
                  "ask"
                  "deny"
                ]
              );
              default = { };
              description = "Per-tool policies (allow, ask, deny).";
              example = {
                run_command = "ask";
                read_file = "allow";
              };
            };

            defaultPolicy = lib.mkOption {
              type = lib.types.enum [
                "allow"
                "ask"
                "deny"
              ];
              default = "allow";
              description = "Default policy for tools not in toolPolicies.";
            };

            approvalTimeoutSeconds = lib.mkOption {
              type = lib.types.int;
              default = 300;
              description = "Timeout for approval requests.";
            };
          };
        };

        delegate = {
          agent = agentOverrideOptions // {
            description = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "Human-readable description of the delegate agent. Surfaced in the delegate_task tool description.";
            };
          };

          extraAgents = lib.mkOption {
            type = lib.types.attrsOf (
              lib.types.submodule {
                options = agentOverrideOptions // {
                  description = lib.mkOption {
                    type = lib.types.nullOr lib.types.str;
                    default = null;
                    description = "Human-readable description of this agent preset. Surfaced in the delegate_task tool's agent parameter description.";
                  };
                };
              }
            );
            default = { };
            description = "Named agent configurations selectable via delegate_task's 'agent' parameter.";
          };

          allowedModels = lib.mkOption {
            type = lib.types.nullOr (lib.types.listOf lib.types.str);
            default = null;
            description = "Models the main agent can choose from via the delegate_task tool parameter. Null disables model selection.";
            example = [
              "claude-haiku-4-20250414"
              "claude-sonnet-4-20250514"
            ];
          };
        };

        maxImageDimension = lib.mkOption {
          type = lib.types.nullOr lib.types.int;
          default = 1568;
          description = "Maximum image dimension in pixels. Images exceeding this are resized before sending to the Claude API. Null disables resizing.";
        };

        toolUseMessages = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Send notification messages when the agent uses tools.";
        };

        compaction = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Enable context compaction. When false, compaction is disabled entirely.";
          };

          tokenThreshold = lib.mkOption {
            type = lib.types.int;
            default = 50000;
            description = "Compact conversation when tokens exceed this.";
          };

          model = lib.mkOption {
            type = lib.types.str;
            default = "claude-3-5-haiku-20241022";
            description = "Model to use for compaction/summarization.";
          };

          prompt = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Custom compaction prompt. Null uses the built-in structured prompt.";
          };

          strategy = lib.mkOption {
            type = strategyType;
            default = {
              keepTurns = 10;
            };
            description = "Strategy for splitting messages into compact vs keep regions.";
          };
        };

        pruning = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Enable pruning. When false, all pruning is disabled.";
          };

          strategy = lib.mkOption {
            type = strategyType;
            default = {
              keepTurns = 3;
            };
            description = "Global default retention strategy for pruning.";
          };

          thinking = {
            enable = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Enable thinking block pruning. When false, all thinking blocks are kept.";
            };

            strategy = lib.mkOption {
              type = lib.types.nullOr strategyType;
              default = null;
              description = "Override strategy for thinking pruning. Null inherits global strategy.";
            };
          };

          tools = {
            enable = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Enable tool pruning. When false, tool inputs/outputs are not pruned.";
            };

            headChars = lib.mkOption {
              type = lib.types.int;
              default = 500;
              description = "Characters to keep from start of pruned content.";
            };

            tailChars = lib.mkOption {
              type = lib.types.int;
              default = 500;
              description = "Characters to keep from end of pruned content.";
            };

            strategy = lib.mkOption {
              type = lib.types.nullOr strategyType;
              default = null;
              description = "Override strategy for all tool pruning. Null inherits global strategy.";
            };

            input = {
              strategy = lib.mkOption {
                type = lib.types.nullOr strategyType;
                default = null;
                description = "Override strategy for tool inputs. Null inherits from tools.";
              };

              headChars = lib.mkOption {
                type = lib.types.nullOr lib.types.int;
                default = null;
                description = "Override head_chars for tool inputs. Null inherits from tools.";
              };

              tailChars = lib.mkOption {
                type = lib.types.nullOr lib.types.int;
                default = null;
                description = "Override tail_chars for tool inputs. Null inherits from tools.";
              };
            };

            output = {
              strategy = lib.mkOption {
                type = lib.types.nullOr strategyType;
                default = null;
                description = "Override strategy for tool outputs. Null inherits from tools.";
              };

              headChars = lib.mkOption {
                type = lib.types.nullOr lib.types.int;
                default = null;
                description = "Override head_chars for tool outputs. Null inherits from tools.";
              };

              tailChars = lib.mkOption {
                type = lib.types.nullOr lib.types.int;
                default = null;
                description = "Override tail_chars for tool outputs. Null inherits from tools.";
              };
            };
          };
        };

        webhook = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable webhook server.";
          };

          port = lib.mkOption {
            type = lib.types.port;
            default = 8080;
            description = "Port for webhook server.";
          };

          endpoints = lib.mkOption {
            type = lib.types.attrsOf webhookEndpointModule;
            default = { };
            description = ''
              Webhook endpoint configurations.
              Prompts support `{{.field}}` template placeholders that are
              substituted with values from the incoming JSON payload
              (e.g. `{{.repo}}`, `{{.branch}}`). Nested fields use dot
              notation (e.g. `{{.commit.author}}`).
            '';
          };
        };

        cronJobs = lib.mkOption {
          type = lib.types.attrsOf cronJobModule;
          default = { };
          description = ''
            Scheduled jobs for this agent.
            Prompts support `{{.field}}` template variables from the cron payload:
            - `{{.time}}` — ISO 8601 timestamp with timezone (e.g. `2026-02-27T08:00:00-05:00`)
            - `{{.trigger}}` — always `"systemd-timer"` for cron jobs
            - `{{.cron}}` — the cron job attribute name (e.g. `"daily-summary"`)
          '';
        };

        mcpServers = lib.mkOption {
          type = lib.types.attrsOf mcpServerModule;
          default = { };
          description = "MCP server configurations.";
        };

        user = lib.mkOption {
          type = lib.types.str;
          default = "assistant";
          description = "User account for this agent.";
        };

        group = lib.mkOption {
          type = lib.types.str;
          default = "assistant";
          description = "Group for this agent.";
        };

        openFirewall = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Open firewall port for webhook server.";
        };
      };
    }
  );

  # Get all enabled agents
  enabledAgents = lib.filterAttrs (_: agent: agent.enable) cfg.agents;

  # Get all cron jobs across all agents (all crons use systemd timers now)
  allTimerCrons = lib.concatMapAttrs (
    agentName: agentCfg:
    lib.mapAttrs' (
      cronName: cronCfg:
      lib.nameValuePair "${agentName}-${cronName}" {
        inherit
          agentName
          cronName
          cronCfg
          agentCfg
          ;
      }
    ) agentCfg.cronJobs
  ) enabledAgents;

in
{
  options.services.assistant = {
    package = lib.mkOption {
      type = lib.types.package;
      default =
        pkgs.elwood
          or (throw "elwood package not found; add the overlay or set services.assistant.package");
      defaultText = lib.literalExpression "pkgs.elwood";
      description = "The elwood package to use.";
    };

    agents = lib.mkOption {
      type = lib.types.attrsOf agentModule;
      default = { };
      description = "Assistant agent configurations.";
      example = lib.literalExpression ''
        {
          elwood = {
            enable = true;
            channels.telegram = [ { id = 123456789; session = "main"; } ];
            environmentFile = "/run/secrets/elwood-env";
          };
          career-coach = {
            enable = true;
            channels.telegram = [ { id = 123456789; } ];
            environmentFile = "/run/secrets/career-coach-env";
            agent.model = "claude-sonnet-4-20250514";
          };
        }
      '';
    };
  };

  config = lib.mkIf (enabledAgents != { }) {
    # Assertions
    assertions =
      let
        # Webhook must be enabled if cron jobs are configured
        webhookAssertions =
          let
            agentsNeedingWebhook = lib.filterAttrs (_: agent: agent.cronJobs != { }) enabledAgents;
          in
          lib.mapAttrsToList (name: agent: {
            assertion = agent.webhook.enable;
            message = ''
              Assistant agent '${name}' has cron jobs configured,
              but webhook.enable is false. Cron jobs require the
              webhook server to be running. Add:

              services.assistant.agents.${name}.webhook.enable = true;
            '';
          }) agentsNeedingWebhook;

        # No two enabled agents may share the same webhook port
        portConflictAssertions =
          let
            agentsWithWebhook = lib.filterAttrs (_: agent: agent.webhook.enable) enabledAgents;
            ports = lib.mapAttrsToList (_: agent: agent.webhook.port) agentsWithWebhook;
          in
          [
            {
              assertion = lib.length ports == lib.length (lib.unique ports);
              message = ''
                Multiple assistant agents are configured with the same webhook port.
                Each agent with webhook.enable must use a unique port.
              '';
            }
          ];

        # Each enabled agent must have at least one telegramChat
        chatIdAssertions = lib.mapAttrsToList (name: agent: {
          assertion = agent.channels.telegram != [ ];
          message = ''
            Assistant agent '${name}' has no channels.telegram configured.
            At least one Telegram chat is required:

            services.assistant.agents.${name}.channels.telegram = [ { id = <your-chat-id>; } ];
          '';
        }) enabledAgents;

        # Delivery target type "telegram" requires non-empty chatIds
        deliveryTargetAssertions =
          let
            collect =
              agentName: agentCfg:
              let
                fromEndpoints = lib.mapAttrsToList (epName: ep: {
                  path = "services.assistant.agents.${agentName}.webhook.endpoints.${epName}.deliveryTarget";
                  dt = ep.deliveryTarget;
                }) agentCfg.webhook.endpoints;
                fromCrons = lib.mapAttrsToList (cronName: cron: {
                  path = "services.assistant.agents.${agentName}.cronJobs.${cronName}.deliveryTarget";
                  dt = cron.deliveryTarget;
                }) agentCfg.cronJobs;
              in
              fromEndpoints ++ fromCrons;
            allTargets = lib.concatLists (lib.mapAttrsToList collect enabledAgents);
          in
          map (t: {
            assertion = t.dt.type != "telegram" || t.dt.chatIds != [ ];
            message = ''
              ${t.path} has type "telegram" but no chatIds.
              At least one chat ID is required:

                ${t.path}.chatIds = [ <chat-id> ];
            '';
          }) allTargets;
        # Each workspace file must have exactly one of text or path
        workspaceFileAssertions = lib.concatLists (
          lib.mapAttrsToList (
            agentName: agentCfg:
            lib.mapAttrsToList (
              fileName: fileCfg:
              let
                hasText = fileCfg.text != null;
                hasPath = fileCfg.path != null;
              in
              {
                assertion = (hasText || hasPath) && !(hasText && hasPath);
                message = ''
                  services.assistant.agents.${agentName}.workspace.files."${fileName}"
                  must have exactly one of 'text' or 'path' set.
                '';
              }
            ) agentCfg.workspace.files
          ) enabledAgents
        );
      in
      webhookAssertions
      ++ portConflictAssertions
      ++ chatIdAssertions
      ++ deliveryTargetAssertions
      ++ workspaceFileAssertions;

    # Collect all unique users and groups
    # Use lib.unique to avoid duplicate definitions when multiple agents share the same user
    users.users =
      let
        # Get unique user names
        uniqueUsers = lib.unique (lib.mapAttrsToList (_: agentCfg: agentCfg.user) enabledAgents);
        # Get the first agent using this user (for home directory)
        firstAgentForUser =
          user: lib.head (lib.filter (agentCfg: agentCfg.user == user) (lib.attrValues enabledAgents));
      in
      lib.listToAttrs (
        map (user: {
          name = user;
          value = {
            isSystemUser = true;
            group = (firstAgentForUser user).group;
            home = (firstAgentForUser user).stateDir;
            description = "Elwood assistant service";
          };
        }) uniqueUsers
      );

    users.groups =
      let
        uniqueGroups = lib.unique (lib.mapAttrsToList (_: agentCfg: agentCfg.group) enabledAgents);
      in
      lib.listToAttrs (
        map (group: {
          name = group;
          value = { };
        }) uniqueGroups
      );

    # Create state directories.  The shared parent must be root-owned so
    # systemd-tmpfiles doesn't reject the root → agent-user ownership
    # transition when multiple agents share the prefix.
    systemd.tmpfiles.rules = [
      "d /var/lib/assistant 0755 root root -"
    ]
    ++ lib.concatMap (agentCfg: [
      "d ${agentCfg.stateDir} 0750 ${agentCfg.user} ${agentCfg.group} -"
      "d ${agentCfg.workspace.path} 0750 ${agentCfg.user} ${agentCfg.group} -"
    ]) (lib.attrValues enabledAgents);

    # Firewall rules
    networking.firewall.allowedTCPPorts = lib.concatMap (
      agentCfg: lib.optional (agentCfg.webhook.enable && agentCfg.openFirewall) agentCfg.webhook.port
    ) (lib.attrValues enabledAgents);

    # Systemd timers for cron jobs
    systemd.timers = lib.mapAttrs' (
      timerName:
      { cronCfg, ... }:
      lib.nameValuePair "assistant-cron-${timerName}" {
        description = "Timer for assistant cron job ${timerName}";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cronCfg.schedule;
          RandomizedDelaySec = "1min";
        };
      }
    ) allTimerCrons;

    # Systemd services (main agents + cron job oneshots)
    systemd.services = lib.mkMerge [
      # Main agent services
      (lib.mapAttrs' (
        name: agentCfg:
        let
          workspaceFilesScript = mkWorkspaceFilesScript agentCfg;
        in
        lib.nameValuePair "assistant-${name}" {
          description = "Elwood Assistant (${name})";
          wantedBy = [ "multi-user.target" ];
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];

          # Extra packages available in PATH for run_command
          path = agentCfg.extraPackages;

          serviceConfig = {
            Type = "simple";
            User = agentCfg.user;
            Group = agentCfg.group;
            ExecStart = lib.getExe cfg.package;
            Restart = "always";
            RestartSec = 10;

            Environment = [
              "ELWOOD_CONFIG=${mkConfigFile name agentCfg}"
            ];
            EnvironmentFile = lib.mkIf (agentCfg.environmentFile != null) agentCfg.environmentFile;

            # Security hardening
            NoNewPrivileges = true;
            ProtectSystem = "strict";
            ProtectHome = true;
            PrivateTmp = true;
            PrivateDevices = true;
            ProtectKernelTunables = true;
            ProtectKernelModules = true;
            ProtectControlGroups = true;
            RestrictAddressFamilies = [
              "AF_INET"
              "AF_INET6"
              "AF_UNIX"
            ];
            RestrictNamespaces = true;
            LockPersonality = true;
            # MemoryDenyWriteExecute breaks Node.js JIT (used by MCP servers)
            MemoryDenyWriteExecute = agentCfg.mcpServers == { };
            RestrictRealtime = true;
            RestrictSUIDSGID = true;
            ReadWritePaths = lib.unique [
              agentCfg.stateDir
              agentCfg.workspace.path
            ];
            StandardOutput = "journal";
            StandardError = "journal";
            SyslogIdentifier = "assistant-${name}";
          }
          // lib.optionalAttrs (workspaceFilesScript != null) {
            ExecStartPre = "${workspaceFilesScript}";
          };
        }
      ) enabledAgents)

      # Cron job oneshot services
      (lib.mapAttrs' (
        timerName:
        {
          agentName,
          cronName,
          cronCfg,
          agentCfg,
        }:
        let
          webhookPort = agentCfg.webhook.port;
          webhookEndpoint = "cron-${cronName}";
        in
        lib.nameValuePair "assistant-cron-${timerName}" {
          description = "Cron job ${cronName} for assistant ${agentName}";
          after = [ "assistant-${agentName}.service" ];
          # Timer-activated oneshots should not be restarted during nixos-rebuild switch;
          # the timer will use the updated service definition on its next firing.
          restartIfChanged = false;

          serviceConfig = {
            Type = "oneshot";
            User = agentCfg.user;
            Group = agentCfg.group;
            ExecStart = pkgs.writeShellScript "assistant-cron-${timerName}" ''
              CURRENT_TIME=$(${pkgs.coreutils}/bin/date -Iseconds)
              PAYLOAD="{\"trigger\": \"systemd-timer\", \"cron\": \"${cronName}\", \"time\": \"$CURRENT_TIME\"}"
              if [ -n "''${WEBHOOK_SECRET:-}" ]; then
                ${pkgs.curl}/bin/curl -sf --retry 3 --retry-delay 2 --retry-connrefused \
                  --connect-timeout 10 -X POST \
                  "http://localhost:${toString webhookPort}/webhook/${webhookEndpoint}" \
                  -H "Content-Type: application/json" \
                  -H "X-Webhook-Secret: $WEBHOOK_SECRET" \
                  -d "$PAYLOAD"
              else
                ${pkgs.curl}/bin/curl -sf --retry 3 --retry-delay 2 --retry-connrefused \
                  --connect-timeout 10 -X POST \
                  "http://localhost:${toString webhookPort}/webhook/${webhookEndpoint}" \
                  -H "Content-Type: application/json" \
                  -d "$PAYLOAD"
              fi
            '';
            StandardOutput = "journal";
            StandardError = "journal";
            SyslogIdentifier = "assistant-cron-${timerName}";
          }
          // lib.optionalAttrs (agentCfg.environmentFile != null) {
            EnvironmentFile = agentCfg.environmentFile;
          };
        }
      ) allTimerCrons)
    ];
  };
}
