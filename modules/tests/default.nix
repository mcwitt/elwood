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
          };

          # Cron jobs now always use systemd timers
          # Webhook endpoints are auto-generated
          cronJobs.daily-job = {
            prompt = [
              {
                type = "text";
                content = "Run daily job";
              }
            ];
            schedule = "*:0/5"; # Every 5 minutes for testing
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

      # Check that the auto-generated webhook endpoint is in the config
      config_path = machine.succeed(
        "systemctl show assistant-cron-test.service -p Environment | grep -oP 'ELWOOD_CONFIG=\\K[^\\s]+'"
      ).strip()
      config = machine.succeed(f"cat {config_path}")
      assert "cron-daily-job" in config, f"Auto-generated cron endpoint not in config: {config}"
    '';
  };

  # Test suppressIfContains for conditional notification
  suppress-if-contains = pkgs.testers.runNixOSTest {
    name = "assistant-suppress-if-contains";

    nodes.machine =
      { ... }:
      {
        imports = [ self.nixosModules.default ];

        services.assistant.package = mockElwood;

        services.assistant.agents.suppress-test = {
          enable = true;
          allowedChatIds = [ 123456789 ];

          webhook = {
            enable = true;
            port = 8080;
          };

          # Heartbeat is now just a regular cron with suppressIfContains
          cronJobs.heartbeat = {
            prompt = [
              {
                type = "text";
                content = "Check system health. Reply HEARTBEAT_OK if all is well.";
              }
            ];
            schedule = "*:0/30";
            suppressIfContains = "HEARTBEAT_OK";
          };
        };
      };

    testScript = ''
      machine.start()
      machine.wait_for_unit("assistant-suppress-test.service")

      # Check cron timer was created
      machine.succeed("systemctl list-timers | grep assistant-cron-suppress-test-heartbeat")

      # Check the config includes suppressIfContains
      config_path = machine.succeed(
        "systemctl show assistant-suppress-test.service -p Environment | grep -oP 'ELWOOD_CONFIG=\\K[^\\s]+'"
      ).strip()
      config = machine.succeed(f"cat {config_path}")
      assert "suppressIfContains" in config, f"suppressIfContains not in config: {config}"
      assert "HEARTBEAT_OK" in config, f"HEARTBEAT_OK pattern not in config: {config}"
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

          # Heartbeat is now handled via systemd timer, not in config
          # We still need webhook enabled for it to work
          webhook = {
            enable = true;
            port = 9000;
          };

          permissions = {
            safePatterns = [
              "^ls\\b"
              "^cat\\b"
            ];
            dangerousPatterns = [ "\\brm\\b" ];
            defaultPolicy = "ask";
            approvalTimeoutSeconds = 600;
          };

          compaction = {
            tokenThreshold = 50000;
            model = "claude-haiku";
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
      # Note: heartbeat config is no longer in the YAML - it's handled via systemd timers
      assert "claude-test-model" in config, f"Model not in config: {config}"
      assert '"tokenThreshold":50000' in config, f"tokenThreshold not in config: {config}"
      assert '"port":9000' in config, f"webhook port not in config: {config}"
      assert '"defaultPolicy":"ask"' in config, f"defaultPolicy not in config: {config}"

      # Verify systemPrompt default is present
      assert "SOUL.md" in config, f"Default systemPrompt (SOUL.md) not in config: {config}"

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

  # Test system prompt with defaultContent creates workspace files
  system-prompt = pkgs.testers.runNixOSTest {
    name = "assistant-system-prompt";

    nodes.machine =
      { ... }:
      {
        imports = [ self.nixosModules.default ];

        services.assistant.package = mockElwood;

        services.assistant.agents.prompt-test = {
          enable = true;
          allowedChatIds = [ 123456789 ];
          stateDir = "/var/lib/assistant/prompt-test";
          workspaceDir = "/var/lib/assistant/prompt-test/workspace";

          systemPrompt = [
            {
              type = "workspaceFile";
              path = "SOUL.md";
              defaultContent = "You are a helpful assistant.";
            }
            {
              type = "text";
              content = "Additional context.";
            }
          ];

          webhook = {
            enable = true;
            port = 8080;
          };
        };
      };

    testScript = ''
      machine.start()
      machine.wait_for_unit("assistant-prompt-test.service")

      # Check that ExecStartPre created the default SOUL.md file
      machine.succeed("test -f /var/lib/assistant/prompt-test/workspace/SOUL.md")
      content = machine.succeed("cat /var/lib/assistant/prompt-test/workspace/SOUL.md")
      assert content == "You are a helpful assistant.", f"Unexpected SOUL.md content: {content}"

      # Check config has systemPrompt with both inputs
      config_path = machine.succeed(
        "systemctl show assistant-prompt-test.service -p Environment | grep -oP 'ELWOOD_CONFIG=\\K[^\\s]+'"
      ).strip()
      config = machine.succeed(f"cat {config_path}")
      assert "SOUL.md" in config, f"SOUL.md not in config: {config}"
      assert "Additional context" in config, f"Inline text not in config: {config}"
    '';
  };
}
