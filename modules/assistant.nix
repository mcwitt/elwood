{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.assistant;

  # Generate YAML config from module options
  configFile = pkgs.writeText "assistant-config.yaml" ''
    stateDir: ${cfg.stateDir}
    workspaceDir: ${cfg.workspaceDir}
    allowedChatIds: [${lib.concatMapStringsSep ", " toString cfg.allowedChatIds}]
    model: ${cfg.model}
    heartbeat:
      intervalMinutes: ${toString cfg.heartbeat.intervalMinutes}
      activeHoursStart: ${toString cfg.heartbeat.activeHoursStart}
      activeHoursEnd: ${toString cfg.heartbeat.activeHoursEnd}
  '';

in
{
  options.services.assistant = {
    enable = lib.mkEnableOption "Elwood personal AI assistant";

    package = lib.mkOption {
      type = lib.types.package;
      default =
        pkgs.elwood
          or (throw "elwood package not found; add the overlay or set services.assistant.package");
      defaultText = lib.literalExpression "pkgs.elwood";
      description = "The elwood package to use.";
    };

    stateDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/assistant";
      description = "Directory for persistent state (conversation history, etc.).";
    };

    workspaceDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/assistant/workspace";
      description = "Directory containing SOUL.md, AGENTS.md, and other workspace files.";
    };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Path to an environment file containing secrets.
        Must contain TELEGRAM_BOT_TOKEN and optionally ANTHROPIC_API_KEY.
      '';
    };

    allowedChatIds = lib.mkOption {
      type = lib.types.listOf lib.types.int;
      default = [ ];
      description = "List of Telegram chat IDs allowed to interact with the bot.";
    };

    model = lib.mkOption {
      type = lib.types.str;
      default = "claude-sonnet-4-20250514";
      description = "Claude model to use for conversations.";
    };

    heartbeat = {
      intervalMinutes = lib.mkOption {
        type = lib.types.int;
        default = 30;
        description = "How often to check for proactive actions (in minutes).";
      };

      activeHoursStart = lib.mkOption {
        type = lib.types.int;
        default = 8;
        description = "Start of active hours (0-23) for proactive messaging.";
      };

      activeHoursEnd = lib.mkOption {
        type = lib.types.int;
        default = 22;
        description = "End of active hours (0-23) for proactive messaging.";
      };
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "assistant";
      description = "User account under which the assistant runs.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "assistant";
      description = "Group under which the assistant runs.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Create system user and group
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.stateDir;
      description = "Elwood AI assistant service user";
    };

    users.groups.${cfg.group} = { };

    # Create state directories
    systemd.tmpfiles.rules = [
      "d ${cfg.stateDir} 0750 ${cfg.user} ${cfg.group} -"
      "d ${cfg.workspaceDir} 0750 ${cfg.user} ${cfg.group} -"
    ];

    # Define the systemd service
    systemd.services.assistant = {
      description = "Elwood Personal AI Assistant";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = "${cfg.package}/bin/elwood";
        Restart = "always";
        RestartSec = 10;

        # Environment
        Environment = [
          "ELWOOD_CONFIG=${configFile}"
        ];
        EnvironmentFile = lib.mkIf (cfg.environmentFile != null) cfg.environmentFile;

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
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;

        # Allow write access to state directory
        ReadWritePaths = [ cfg.stateDir ];

        # Logging
        StandardOutput = "journal";
        StandardError = "journal";
        SyslogIdentifier = "assistant";
      };
    };
  };
}
