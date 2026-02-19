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
      mkDeliverTarget =
        dt:
        {
          type = dt.type;
        }
        // lib.optionalAttrs (dt.session != null) {
          session = dt.session;
        };

      allWebhookEndpoints =
        let
          # User-defined endpoints
          userEndpoints = lib.mapAttrsToList (
            epName: epCfg:
            {
              name = epName;
              promptTemplate = epCfg.promptTemplate;
              session = epCfg.session;
              deliver = map mkDeliverTarget epCfg.deliver;
            }
            // lib.optionalAttrs (epCfg.secret != null) {
              secret = epCfg.secret;
            }
            // lib.optionalAttrs (epCfg.suppressIfContains != null) {
              suppressIfContains = epCfg.suppressIfContains;
            }
          ) agentCfg.webhook.endpoints;

          # Auto-generated cron job endpoints
          cronEndpoints = lib.mapAttrsToList (
            cronName: cronCfg:
            {
              name = "cron-${cronName}";
              session = cronCfg.session;
              deliver = map mkDeliverTarget cronCfg.deliver;
            }
            // lib.optionalAttrs (cronCfg.prompt != null) {
              promptTemplate = cronCfg.prompt;
            }
            // lib.optionalAttrs (cronCfg.promptFile != null) {
              promptFile = cronCfg.promptFile;
            }
            // lib.optionalAttrs (cronCfg.webhookSecret != null) {
              secret = cronCfg.webhookSecret;
            }
            // lib.optionalAttrs (cronCfg.suppressIfContains != null) {
              suppressIfContains = cronCfg.suppressIfContains;
            }
          ) agentCfg.cronJobs;
        in
        userEndpoints ++ cronEndpoints;

      webhookEndpoints = allWebhookEndpoints;

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
          startupDelay = serverCfg.startupDelay;
        }
      ) agentCfg.mcpServers;

      configContent = {
        stateDir = agentCfg.stateDir;
        workspaceDir = agentCfg.workspaceDir;
        allowedChatIds = agentCfg.allowedChatIds;
        model = agentCfg.model;
        maxHistory = agentCfg.maxHistory;
        thinking = agentCfg.thinking;

        # Heartbeat and cronJobs are now handled externally via systemd timers
        # The Haskell app no longer needs these config sections

        permissions = {
          safeCommands = agentCfg.permissions.safeCommands;
          dangerousPatterns = agentCfg.permissions.dangerousPatterns;
          allowedPaths = agentCfg.permissions.allowedPaths;
          toolPolicies = agentCfg.permissions.toolPolicies;
          defaultPolicy = agentCfg.permissions.defaultPolicy;
          approvalTimeoutSeconds = agentCfg.permissions.approvalTimeoutSeconds;
        };

        compaction = {
          tokenThreshold = agentCfg.compaction.tokenThreshold;
          compactionModel = agentCfg.compaction.model;
        };
      }
      // lib.optionalAttrs (mcpServersList != { }) {
        mcpServers = mcpServersList;
      }
      // lib.optionalAttrs agentCfg.webhook.enable {
        webhook = {
          enabled = true;
          port = agentCfg.webhook.port;
        }
        // lib.optionalAttrs (agentCfg.webhook.globalSecret != null) {
          globalSecret = agentCfg.webhook.globalSecret;
        }
        // lib.optionalAttrs (webhookEndpoints != [ ]) {
          endpoints = webhookEndpoints;
        };
      };
    in
    pkgs.writeText "assistant-${name}-config.yaml" (lib.generators.toYAML { } configContent);

  # Submodule for delivery targets
  deliverTargetModule = lib.types.submodule {
    options = {
      type = lib.mkOption {
        type = lib.types.enum [
          "telegram"
          "log"
        ];
        description = "Delivery target type.";
      };

      session = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Telegram chat ID to send to. If null, broadcasts to all allowedChatIds.";
        example = "123456789";
      };
    };
  };

  # Submodule for webhook endpoints
  webhookEndpointModule = lib.types.submodule {
    options = {
      secret = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Secret for this endpoint (overrides global secret).";
      };

      promptTemplate = lib.mkOption {
        type = lib.types.str;
        description = "Prompt template with {{.field}} placeholders.";
      };

      session = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Session name for persistent conversation. Null means isolated (no history).";
      };

      deliver = lib.mkOption {
        type = lib.types.listOf deliverTargetModule;
        default = [ { type = "telegram"; } ];
        description = "Delivery targets.";
      };

      suppressIfContains = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Suppress notification if response contains this string.";
        example = "HEARTBEAT_OK";
      };
    };
  };

  # Submodule for cron jobs (always use systemd timers)
  cronJobModule = lib.types.submodule {
    options = {
      prompt = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Prompt to send to the agent. Mutually exclusive with promptFile.";
      };

      promptFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "File in workspaceDir to read prompt from at runtime. Mutually exclusive with prompt.";
        example = "HEARTBEAT.md";
      };

      session = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Session name for persistent conversation. Null means isolated (no history). Use a Telegram chat ID to share conversation with that chat.";
        example = "123456789";
      };

      deliver = lib.mkOption {
        type = lib.types.listOf deliverTargetModule;
        default = [ { type = "telegram"; } ];
        description = "Delivery targets.";
      };

      schedule = lib.mkOption {
        type = lib.types.str;
        default = "hourly";
        description = "systemd OnCalendar schedule.";
        example = "*-*-* 09:00";
      };

      webhookSecret = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Secret for the auto-generated webhook endpoint. Falls back to global webhook secret.";
      };

      suppressIfContains = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Suppress notification if response contains this string.";
        example = "HEARTBEAT_OK";
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

        maxHistory = lib.mkOption {
          type = lib.types.int;
          default = 50;
          description = "Maximum messages to keep per conversation.";
        };

        thinking = lib.mkOption {
          type = lib.types.enum [
            "off"
            "low"
            "medium"
            "high"
          ];
          default = "off";
          description = "Extended thinking level. Controls Claude's internal reasoning budget before responding.";
        };

        permissions = {
          safeCommands = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Commands that are always allowed (prefix match).";
          };

          dangerousPatterns = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Regex patterns that are always blocked.";
          };

          allowedPaths = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Paths allowed for file operations.";
          };

          toolPolicies = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = { };
            description = "Per-tool policies (allow, ask, deny).";
            example = {
              run_command = "ask";
              read_file = "allow";
            };
          };

          defaultPolicy = lib.mkOption {
            type = lib.types.str;
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
            default = 80000;
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

          globalSecret = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Global secret for all webhook endpoints.";
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

        # Cron jobs must have exactly one of prompt or promptFile
        cronAssertions = lib.concatLists (
          lib.mapAttrsToList (
            agentName: agentCfg:
            lib.mapAttrsToList (cronName: cronCfg: {
              assertion = (cronCfg.prompt != null) != (cronCfg.promptFile != null);
              message = ''
                Cron job '${cronName}' in agent '${agentName}' must have exactly one
                of 'prompt' or 'promptFile' set, not both or neither.
              '';
            }) agentCfg.cronJobs
          ) enabledAgents
        );
      in
      webhookAssertions ++ cronAssertions;

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
            ExecStart = "${cfg.package}/bin/elwood";
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
            # Note: MemoryDenyWriteExecute breaks Node.js JIT (used by MCP servers)
            MemoryDenyWriteExecute = false;
            RestrictRealtime = true;
            RestrictSUIDSGID = true;
            ReadWritePaths = [ agentCfg.stateDir ];
            StandardOutput = "journal";
            StandardError = "journal";
            SyslogIdentifier = "assistant-${name}";
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
          secret =
            if cronCfg.webhookSecret != null then cronCfg.webhookSecret else agentCfg.webhook.globalSecret;
          secretHeader = lib.optionalString (secret != null) ''-H "X-Webhook-Secret: ${secret}"'';
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
              ${pkgs.curl}/bin/curl -s -X POST \
                "http://localhost:${toString webhookPort}/webhook/${webhookEndpoint}" \
                -H "Content-Type: application/json" \
                ${secretHeader} \
                -d '{"trigger": "systemd-timer", "cron": "${cronName}"}'
            '';
            StandardOutput = "journal";
            StandardError = "journal";
            SyslogIdentifier = "assistant-cron-${timerName}";
          };
        }
      ) allTimerCrons)
    ];

    # Cron job webhook endpoints are now auto-created in the config generation
    # so no warnings are needed
  };
}
