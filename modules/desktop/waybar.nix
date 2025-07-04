{
  config,
  pkgs,
  ...
}: {
  # Ensure waybar is installed
  environment.systemPackages = with pkgs; [waybar];

  programs.waybar = {
    enable = true;
    settings = {
      layer = "top";
      position = "top";
      height = 30;
      spacing = 4;
      modules-left = ["sway/workspaces"];
      modules-center = [];
      modules-right = ["network" "clock"];
      "sway/workspaces" = {
        disable-scroll = false;
        all-outputs = true;
        format = "{name}";
        format-icons = {
          "1" = "1";
          "2" = "2";
          "3" = "3";
          "4" = "4";
          "5" = "5";
          urgent = "";
          focused = "";
          default = "";
        };
      };
      clock = {
        timezone = "Europe/Oslo";
        format = "{:%H:%M}";
        format-alt = "{:%Y-%m-%d %H:%M:%S}";
        tooltip-format = "<tt><small>{calendar}</small></tt>";
        calendar = {
          mode = "year";
          mode-mon-col = 3;
          weeks-pos = "right";
          on-scroll = 1;
          on-click-right = "mode";
          format = {
            months = "<span color='#ffead3'><b>{}</b></span>";
            days = "<span color='#ecc6d9'><b>{}</b></span>";
            weeks = "<span color='#99ffdd'><b>W{}</b></span>";
            weekdays = "<span color='#ffcc66'><b>{}</b></span>";
            today = "<span color='#ff6699'><b><u>{}</u></b></span>";
          };
        };
        actions = {
          on-click-right = "mode";
          on-click-forward = "tz_up";
          on-click-backward = "tz_down";
          on-scroll-up = "shift_up";
          on-scroll-down = "shift_down";
        };
      };
      network = {
        format-wifi = "{essid} ({signalStrength}%) ";
        format-ethernet = "{ipaddr}/{cidr} ";
        tooltip-format = "{ifname} via {gwaddr} ";
        format-linked = "{ifname} (No IP) ";
        format-disconnected = "Disconnected ⚠";
        format-alt = "{ifname}: {ipaddr}/{cidr}";
      };
    };
    style = ''
      * {
          border: none;
          border-radius: 0;
          font-family: 'Inter', 'Font Awesome 6 Free';
          font-size: 13px;
          min-height: 0;
      }

      window#waybar {
          background-color: rgba(43, 48, 59, 0.9);
          border-bottom: 3px solid rgba(100, 114, 125, 0.5);
          color: #ffffff;
          transition-property: background-color;
          transition-duration: 0.5s;
      }

      window#waybar.hidden {
          opacity: 0.2;
      }

      #workspaces {
          margin: 0 4px;
      }

      #workspaces button {
          padding: 0 8px;
          background-color: transparent;
          color: #ffffff;
          border-bottom: 3px solid transparent;
          transition: all 0.3s ease;
      }

      #workspaces button:hover {
          background: rgba(0, 0, 0, 0.2);
      }

      #workspaces button.focused {
          background-color: #64727D;
          border-bottom: 3px solid #ffffff;
      }

      #workspaces button.urgent {
          background-color: #eb4d4b;
          color: #ffffff;
      }

      #clock,
      #network {
          padding: 0 10px;
          color: #ffffff;
      }

      #clock {
          background-color: #64727D;
          border-radius: 0 8px 8px 0;
      }

      #network {
          background-color: #2980b9;
          border-radius: 8px 0 0 8px;
      }

      #network.disconnected {
          background-color: #f53c3c;
      }

      @keyframes blink {
          to {
              background-color: #ffffff;
              color: #000000;
          }
      }

      #network.disconnected {
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }
    '';
  };
}
