{
  pkgs,
  self,
}:

let
  # Create a mock elwood package that just sleeps forever
  # This avoids needing real API keys in tests - we just verify
  # the NixOS module creates services/configs correctly
  mockElwood = pkgs.writeShellScriptBin "elwood" ''
    echo "Mock elwood starting"
    echo "Config: $ELWOOD_CONFIG"
    # Just sleep forever (simulating a running service)
    exec sleep infinity
  '';

in
{
  # Test single agent configuration
  single-agent = pkgs.testers.runNixOSTest {
    name = "assistant-single-agent";

    nodes.machine =
      { ... }:
      {
        imports = [ self.nixosModules.default ];

        services.assistant.package = mockElwood;

        services.assistant.agents.test-agent = {
          enable = true;
          allowedChatIds = [ 123456789 ];
          stateDir = "/var/lib/assistant/test-agent";
          workspaceDir = "/var/lib/assistant/test-agent/workspace";

          webhook = {
            enable = true;
            port = 8080;
          };
        };
      };

    testScript = ''
      machine.start()
      machine.wait_for_unit("assistant-test-agent.service")

      # Check service is running
      machine.succeed("systemctl is-active assistant-test-agent.service")

      # Check state directories were created
      machine.succeed("test -d /var/lib/assistant/test-agent")
      machine.succeed("test -d /var/lib/assistant/test-agent/workspace")

      # Check user was created
      machine.succeed("id assistant")

      # Check config file exists (via environment variable in service)
      config_path = machine.succeed(
        "systemctl show assistant-test-agent.service -p Environment | grep -oP 'ELWOOD_CONFIG=\\K[^\\s]+'"
      ).strip()
      machine.succeed(f"test -f {config_path}")
    '';
  };

  # Test multiple agents
  multi-agent = pkgs.testers.runNixOSTest {
    name = "assistant-multi-agent";

    nodes.machine =
      { ... }:
      {
        imports = [ self.nixosModules.default ];

        services.assistant.package = mockElwood;

        services.assistant.agents = {
          agent-one = {
            enable = true;
            allowedChatIds = [ 111111111 ];
            webhook = {
              enable = true;
              port = 8081;
            };
          };

          agent-two = {
            enable = true;
            allowedChatIds = [ 222222222 ];
            webhook = {
              enable = true;
              port = 8082;
            };
          };
        };
      };

    testScript = ''
      machine.start()

      # Wait for both services
      machine.wait_for_unit("assistant-agent-one.service")
      machine.wait_for_unit("assistant-agent-two.service")

      # Check both are running
      machine.succeed("systemctl is-active assistant-agent-one.service")
      machine.succeed("systemctl is-active assistant-agent-two.service")

      # Check separate state directories
      machine.succeed("test -d /var/lib/assistant/agent-one")
      machine.succeed("test -d /var/lib/assistant/agent-two")

      # Check config files exist for both agents
      config1 = machine.succeed(
        "systemctl show assistant-agent-one.service -p Environment | grep -oP 'ELWOOD_CONFIG=\\K[^\\s]+'"
      ).strip()
      config2 = machine.succeed(
        "systemctl show assistant-agent-two.service -p Environment | grep -oP 'ELWOOD_CONFIG=\\K[^\\s]+'"
      ).strip()
      machine.succeed(f"test -f {config1}")
      machine.succeed(f"test -f {config2}")
    '';
  };

  # Test systemd timer cron jobs
  cron-timers = pkgs.testers.runNixOSTest {
    name = "assistant-cron-timers";

    nodes.machine =
      { ... }:
      {
        imports = [ self.nixosModules.default ];

        services.assistant.package = mockElwood;

        services.assistant.agents.cron-test = {
          enable = true;
          allowedChatIds = [ 123456789 ];

          webhook = {
            enable = true;
            port = 8080;
            globalSecret = "test-secret";
            endpoints."cron-daily-job" = {
              promptTemplate = "Run daily job";
              session = "isolated";
              deliver = [ "telegram" ];
            };
          };

          cronJobs.daily-job = {
            prompt = "Run daily job";
            useSystemdTimer = true;
            schedule = "*:0/5"; # Every 5 minutes for testing
            isolated = true;
          };
        };
      };

    testScript = ''
      machine.start()
      machine.wait_for_unit("assistant-cron-test.service")

      # Check timer was created
      machine.succeed("systemctl list-timers | grep assistant-cron-cron-test-daily-job")

      # Check timer is active
      machine.succeed("systemctl is-active assistant-cron-cron-test-daily-job.timer")

      # Check the oneshot service exists (but isn't running yet)
      machine.succeed("systemctl cat assistant-cron-cron-test-daily-job.service")
    '';
  };

  # Test config generation
  config-generation = pkgs.testers.runNixOSTest {
    name = "assistant-config-generation";

    nodes.machine =
      { ... }:
      {
        imports = [ self.nixosModules.default ];

        services.assistant.package = mockElwood;

        services.assistant.agents.config-test = {
          enable = true;
          allowedChatIds = [
            111
            222
          ];
          model = "claude-test-model";
          maxHistory = 100;

          heartbeat = {
            enable = true;
            intervalMinutes = 60;
            activeHoursStart = 9;
            activeHoursEnd = 21;
          };

          permissions = {
            safeCommands = [
              "ls"
              "cat"
            ];
            dangerousPatterns = [ "rm -rf" ];
            defaultPolicy = "ask";
            approvalTimeoutSeconds = 600;
          };

          compaction = {
            tokenThreshold = 50000;
            model = "claude-haiku";
          };

          webhook = {
            enable = true;
            port = 9000;
          };
        };
      };

    testScript = ''
      machine.start()
      machine.wait_for_unit("assistant-config-test.service")

      # Get the config file path from the service
      config_path = machine.succeed(
        "systemctl show assistant-config-test.service -p Environment | grep -oP 'ELWOOD_CONFIG=\\K[^\\s]+'"
      ).strip()

      # Read and verify config contents (lib.generators.toYAML produces JSON)
      config = machine.succeed(f"cat {config_path}")

      # Verify key config values are present (JSON format)
      assert "claude-test-model" in config, f"Model not in config: {config}"
      assert '"maxHistory":100' in config, f"maxHistory not in config: {config}"
      assert '"intervalMinutes":60' in config, f"heartbeat interval not in config: {config}"
      assert '"activeHoursStart":9' in config, f"activeHoursStart not in config: {config}"
      assert '"tokenThreshold":50000' in config, f"tokenThreshold not in config: {config}"
      assert '"port":9000' in config, f"webhook port not in config: {config}"
      assert '"defaultPolicy":"ask"' in config, f"defaultPolicy not in config: {config}"

      print("Config validation passed!")
      print(config)
    '';
  };

  # Test firewall integration
  firewall = pkgs.testers.runNixOSTest {
    name = "assistant-firewall";

    nodes.machine =
      { ... }:
      {
        imports = [ self.nixosModules.default ];

        networking.firewall.enable = true;

        services.assistant.package = mockElwood;

        services.assistant.agents = {
          # This one should open the firewall
          open-firewall = {
            enable = true;
            allowedChatIds = [ 123 ];
            webhook = {
              enable = true;
              port = 8080;
            };
            openFirewall = true;
          };

          # This one should NOT open the firewall
          closed-firewall = {
            enable = true;
            allowedChatIds = [ 456 ];
            webhook = {
              enable = true;
              port = 8081;
            };
            openFirewall = false;
          };
        };
      };

    testScript = ''
      machine.start()
      machine.wait_for_unit("assistant-open-firewall.service")
      machine.wait_for_unit("assistant-closed-firewall.service")

      # Check firewall rules (NixOS uses nixos-fw chain)
      rules = machine.succeed("iptables -L nixos-fw -n")

      # Port 8080 should be open
      assert "8080" in rules, f"Port 8080 should be open in firewall: {rules}"

      # Port 8081 should NOT be in the rules (closed)
      assert "8081" not in rules, f"Port 8081 should NOT be open in firewall: {rules}"
    '';
  };
}
