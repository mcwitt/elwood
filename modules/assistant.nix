{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.assistant;

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

      # Serialize a prompt input, stripping the NixOS-only defaultContent field
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
        // lib.optionalAttrs (epCfg.model != null) {
          model = epCfg.model;
        }
        // lib.optionalAttrs (epCfg.thinking != null) {
          thinking = mkThinkingConfig epCfg.thinking;
        };

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
        workspace_dir = agentCfg.workspaceDir;
        allowed_chat_ids = agentCfg.allowedChatIds;
        model = agentCfg.model;
        thinking = mkThinkingConfig agentCfg.thinking;
        max_iterations = agentCfg.maxIterations;
        system_prompt = map mkPromptInputYaml agentCfg.systemPrompt;

        permissions = {
          safe_patterns = agentCfg.permissions.safePatterns;
          dangerous_patterns = agentCfg.permissions.dangerousPatterns;
          tool_policies = agentCfg.permissions.toolPolicies;
          default_policy = agentCfg.permissions.defaultPolicy;
          approval_timeout_seconds = agentCfg.permissions.approvalTimeoutSeconds;
        };

        compaction = {
          token_threshold = agentCfg.compaction.tokenThreshold;
          model = agentCfg.compaction.model;
        };
      }
      // lib.optionalAttrs (agentCfg.toolSearch != null) {
        tool_search = agentCfg.toolSearch;
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
      };
    in
    pkgs.writeText "assistant-${name}-config.yaml" (lib.generators.toYAML { } configContent);

  # Collect all workspaceFile inputs with defaultContent from an agent config
  collectDefaultContentFiles =
    agentCfg:
    let
      fromInputs =
        inputs:
        lib.filter (pi: pi.type == "workspace_file" && pi.path != null && pi.defaultContent != null) inputs;

      systemPromptFiles = fromInputs agentCfg.systemPrompt;

      webhookFiles = lib.concatMap (epCfg: fromInputs epCfg.prompt) (
        lib.attrValues agentCfg.webhook.endpoints
      );

      cronFiles = lib.concatMap (cronCfg: fromInputs cronCfg.prompt) (lib.attrValues agentCfg.cronJobs);
    in
    systemPromptFiles ++ webhookFiles ++ cronFiles;

  # Generate ExecStartPre script for creating default workspace files
  mkDefaultContentScript =
    agentCfg:
    let
      files = collectDefaultContentFiles agentCfg;
      mkFileCheck =
        pi:
        let
          fullPath = "${agentCfg.workspaceDir}/${pi.path}";
        in
        ''
          if [ ! -f ${lib.escapeShellArg fullPath} ]; then
            printf '%s' ${lib.escapeShellArg pi.defaultContent} > ${lib.escapeShellArg fullPath}
          fi
        '';
    in
    if files == [ ] then
      null
    else
      pkgs.writeShellScript "assistant-default-content" (
        lib.concatMapStrings mkFileCheck (lib.unique files)
      );

  # Submodule for thinking configuration
  thinkingModule = lib.types.submodule {
    options = {
      type = lib.mkOption {
        type = lib.types.enum [
          "off"
          "adaptive"
          "fixed"
        ];
        default = "off";
        description = "Thinking type: off, adaptive, or fixed.";
        example = "adaptive";
      };

      effort = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "low"
            "medium"
            "high"
          ]
        );
        default = null;
        description = "Effort level for adaptive thinking.";
        example = "medium";
      };

      budgetTokens = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = "Fixed budget_tokens value for older models.";
      };
    };
  };

  # Helper to serialize a thinking config submodule to a YAML-ready value
  # "off" emits false (YAML: `thinking: false`, parsed as ThinkingOff)
  mkThinkingConfig =
    tc:
    if tc.type == "off" then
      false
    else
      {
        inherit (tc) type;
      }
      // lib.optionalAttrs (tc.effort != null) {
        inherit (tc) effort;
      }
      // lib.optionalAttrs (tc.budgetTokens != null) {
        budget_tokens = tc.budgetTokens;
      };

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
        description = "File path relative to workspaceDir (workspaceFile type only).";
      };

      content = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Inline text content (text type only).";
      };

      defaultContent = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Create file with this content if missing (NixOS-only, workspaceFile type only).";
      };
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

    model = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Model override for this endpoint. Null means use the agent's global model.";
      example = "claude-haiku-4-20250414";
    };

    thinking = lib.mkOption {
      type = lib.types.nullOr thinkingModule;
      default = null;
      description = "Thinking override for this endpoint. Null means use the agent's global thinking.";
    };
  };

  # Submodule for webhook endpoints
  webhookEndpointModule = lib.types.submodule {
    options = commonEndpointOptions;
  };

  # Submodule for cron jobs (always use systemd timers)
  cronJobModule = lib.types.submodule {
    options = commonEndpointOptions // {
      schedule = lib.mkOption {
        type = lib.types.str;
        default = "hourly";
        description = "systemd OnCalendar schedule.";
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

        workspaceDir = lib.mkOption {
          type = lib.types.path;
          default = "/var/lib/assistant/${name}/workspace";
          description = "Directory containing SOUL.md and other workspace files.";
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

        allowedChatIds = lib.mkOption {
          type = lib.types.listOf lib.types.int;
          default = [ ];
          description = "Telegram chat IDs allowed to interact with this agent.";
        };

        model = lib.mkOption {
          type = lib.types.str;
          default = "claude-sonnet-4-20250514";
          description = "Claude model to use.";
        };

        thinking = lib.mkOption {
          type = thinkingModule;
          default = { };
          description = ''
            Extended thinking configuration. Default is off.
            Set type to "adaptive" with an effort level, or "fixed" with budgetTokens.
          '';
        };

        maxIterations = lib.mkOption {
          type = lib.types.int;
          default = 20;
          description = "Maximum agent loop iterations per turn (prevents infinite tool-use loops).";
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
            default = "ask";
            description = "Default policy for tools not in toolPolicies.";
          };

          approvalTimeoutSeconds = lib.mkOption {
            type = lib.types.int;
            default = 300;
            description = "Timeout for approval requests.";
          };
        };

        compaction = {
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
            description = "Webhook endpoint configurations.";
          };
        };

        cronJobs = lib.mkOption {
          type = lib.types.attrsOf cronJobModule;
          default = { };
          description = "Scheduled jobs for this agent.";
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
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/elwood-env";
          };
          career-coach = {
            enable = true;
            allowedChatIds = [ 123456789 ];
            environmentFile = "/run/secrets/career-coach-env";
            model = "claude-sonnet-4-20250514";
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

        # Each enabled agent must have at least one allowedChatId
        chatIdAssertions = lib.mapAttrsToList (name: agent: {
          assertion = agent.allowedChatIds != [ ];
          message = ''
            Assistant agent '${name}' has no allowedChatIds configured.
            At least one Telegram chat ID is required:

            services.assistant.agents.${name}.allowedChatIds = [ <your-chat-id> ];
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
      in
      webhookAssertions ++ portConflictAssertions ++ chatIdAssertions ++ deliveryTargetAssertions;

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

    # Create state directories
    systemd.tmpfiles.rules = lib.concatMap (agentCfg: [
      "d ${agentCfg.stateDir} 0750 ${agentCfg.user} ${agentCfg.group} -"
      "d ${agentCfg.workspaceDir} 0750 ${agentCfg.user} ${agentCfg.group} -"
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
          Persistent = true;
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
          defaultContentScript = mkDefaultContentScript agentCfg;
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
              agentCfg.workspaceDir
            ];
            StandardOutput = "journal";
            StandardError = "journal";
            SyslogIdentifier = "assistant-${name}";
          }
          // lib.optionalAttrs (defaultContentScript != null) {
            ExecStartPre = "${defaultContentScript}";
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
          requires = [ "assistant-${agentName}.service" ];

          serviceConfig = {
            Type = "oneshot";
            User = agentCfg.user;
            Group = agentCfg.group;
            ExecStart = pkgs.writeShellScript "assistant-cron-${timerName}" ''
              if [ -n "''${WEBHOOK_SECRET:-}" ]; then
                ${pkgs.curl}/bin/curl -s -X POST \
                  "http://localhost:${toString webhookPort}/webhook/${webhookEndpoint}" \
                  -H "Content-Type: application/json" \
                  -H "X-Webhook-Secret: $WEBHOOK_SECRET" \
                  -d '{"trigger": "systemd-timer", "cron": "${cronName}"}'
              else
                ${pkgs.curl}/bin/curl -s -X POST \
                  "http://localhost:${toString webhookPort}/webhook/${webhookEndpoint}" \
                  -H "Content-Type: application/json" \
                  -d '{"trigger": "systemd-timer", "cron": "${cronName}"}'
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
