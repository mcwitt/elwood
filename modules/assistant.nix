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
      # Build cron jobs list (internal crons, not systemd-managed ones)
      cronJobsList = lib.mapAttrsToList (cronName: cronCfg: {
        name = cronName;
        intervalMinutes = cronCfg.intervalMinutes;
        prompt = cronCfg.prompt;
        isolated = cronCfg.isolated;
      }) (lib.filterAttrs (_: c: !c.useSystemdTimer) agentCfg.cronJobs);

      # Build webhook endpoints
      webhookEndpoints = lib.mapAttrsToList (
        epName: epCfg:
        {
          name = epName;
          promptTemplate = epCfg.promptTemplate;
          session = epCfg.session;
          deliver = epCfg.deliver;
        }
        // lib.optionalAttrs (epCfg.secret != null) {
          secret = epCfg.secret;
        }
      ) agentCfg.webhook.endpoints;

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

        heartbeat = {
          enabled = agentCfg.heartbeat.enable;
          intervalMinutes = agentCfg.heartbeat.intervalMinutes;
          activeHoursStart = agentCfg.heartbeat.activeHoursStart;
          activeHoursEnd = agentCfg.heartbeat.activeHoursEnd;
        };

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
      // lib.optionalAttrs (cronJobsList != [ ]) {
        cronJobs = cronJobsList;
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
        type = lib.types.str;
        default = "isolated";
        description = "Session mode: 'isolated' or 'named:<id>'.";
      };

      deliver = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "telegram" ];
        description = "Delivery targets: telegram, log.";
      };
    };
  };

  # Submodule for cron jobs
  cronJobModule = lib.types.submodule {
    options = {
      prompt = lib.mkOption {
        type = lib.types.str;
        description = "Prompt to send to the agent.";
      };

      intervalMinutes = lib.mkOption {
        type = lib.types.int;
        default = 60;
        description = "Interval in minutes (used if useSystemdTimer is false).";
      };

      isolated = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to use an isolated session for each run.";
      };

      useSystemdTimer = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Use systemd timer instead of internal scheduler.";
      };

      schedule = lib.mkOption {
        type = lib.types.str;
        default = "*:0/60";
        description = "systemd OnCalendar schedule (only used if useSystemdTimer is true).";
        example = "daily";
      };

      webhookSecret = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Secret for the auto-generated webhook endpoint.";
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

        heartbeat = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Enable heartbeat/proactive checks.";
          };

          intervalMinutes = lib.mkOption {
            type = lib.types.int;
            default = 30;
            description = "Heartbeat check interval in minutes.";
          };

          activeHoursStart = lib.mkOption {
            type = lib.types.int;
            default = 8;
            description = "Start of active hours (0-23).";
          };

          activeHoursEnd = lib.mkOption {
            type = lib.types.int;
            default = 22;
            description = "End of active hours (0-23).";
          };
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

  # Get all systemd timer crons across all agents
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
    ) (lib.filterAttrs (_: c: c.useSystemdTimer) agentCfg.cronJobs)
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
            MemoryDenyWriteExecute = true;
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
          sessionMode = if cronCfg.isolated then "isolated" else "named:cron-${cronName}";
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

    # Auto-create webhook endpoints for systemd timer crons
    # This is a bit tricky - we need to inject these into the agent configs
    # For now, users need to manually add the webhook endpoints for systemd timer crons
    warnings =
      let
        timerCronsWithoutWebhook = lib.filterAttrs (
          timerName:
          {
            agentName,
            cronName,
            agentCfg,
            ...
          }:
          agentCfg.webhook.enable && !(lib.hasAttr "cron-${cronName}" agentCfg.webhook.endpoints)
        ) allTimerCrons;
      in
      lib.mapAttrsToList (
        timerName:
        {
          agentName,
          cronName,
          cronCfg,
          ...
        }:
        ''
          Assistant agent '${agentName}' has cron job '${cronName}' with useSystemdTimer=true,
          but no webhook endpoint 'cron-${cronName}' is defined. Add this to your config:

          services.assistant.agents.${agentName}.webhook.endpoints."cron-${cronName}" = {
            promptTemplate = '''
              ${cronCfg.prompt}
            ''';
            session = "${if cronCfg.isolated then "isolated" else "named:cron-${cronName}"}";
            deliver = [ "telegram" ];
          };
        ''
      ) timerCronsWithoutWebhook;
  };
}
