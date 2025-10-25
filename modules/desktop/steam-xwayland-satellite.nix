{
  config,
  pkgs,
  ...
}: {
  # Steam configuration with xwayland-satellite support

  # Create a wrapper script for Steam that uses xwayland-satellite
  environment.systemPackages = with pkgs; [
    (writeShellScriptBin "steam-xwayland-satellite" ''
      #!/usr/bin/env bash

      # Set environment variables for better Steam compatibility
      export STEAM_FRAME_FORCE_CLOSE=1
      export DXVK_HUD=fps
      export __GL_THREADED_OPTIMIZATIONS=1

      # Check if xwayland-satellite is running and get its DISPLAY
      XWAYLAND_PID=$(pgrep -f "xwayland-satellite")
      if [ -z "$XWAYLAND_PID" ]; then
        echo "Starting xwayland-satellite..."
        ${xwayland-satellite}/bin/xwayland-satellite &
        sleep 5  # Give it more time to start
        XWAYLAND_PID=$(pgrep -f "xwayland-satellite")
        if [ -z "$XWAYLAND_PID" ]; then
          echo "Warning: Failed to start xwayland-satellite"
        fi
      fi

      # Debug: Show what displays are available
      echo "Available X11 sockets:"
      ls -la /tmp/.X11-unix/ 2>/dev/null || echo "No X11 sockets found"

      # Try to detect the DISPLAY from xwayland-satellite
      if [ -n "$XWAYLAND_PID" ]; then
        # Try to find DISPLAY from the process environment
        XWAYLAND_DISPLAY=$(tr '\0' '\n' < /proc/$XWAYLAND_PID/environ 2>/dev/null | grep '^DISPLAY=' | cut -d= -f2)
        if [ -n "$XWAYLAND_DISPLAY" ]; then
          export DISPLAY="$XWAYLAND_DISPLAY"
          echo "Found xwayland-satellite on DISPLAY=$DISPLAY"
        else
          # Look for available X displays (prioritize :1 since that's where xwayland-satellite typically runs)
          for display in :1 :2 :3 :0; do
            if [ -S "/tmp/.X11-unix/X''${display#:}" ]; then
              export DISPLAY="$display"
              echo "Using available X display: $DISPLAY"
              break
            fi
          done
        fi
      else
        echo "Warning: xwayland-satellite not found, checking for available displays..."
        # Look for available X displays (prioritize :1)
        for display in :1 :2 :3 :0; do
          if [ -S "/tmp/.X11-unix/X''${display#:}" ]; then
            export DISPLAY="$display"
            echo "Using available X display: $DISPLAY"
            break
          fi
        done
      fi

      # Final fallback - if no DISPLAY is set, try :1 explicitly
      if [ -z "$DISPLAY" ]; then
        echo "No DISPLAY found, trying :1 as fallback..."
        export DISPLAY=":1"
      fi

      echo "Starting Steam with xwayland-satellite on DISPLAY=$DISPLAY"
      exec ${steam}/bin/steam "$@"
    '')

    # Also create a desktop entry for the launcher
    (makeDesktopItem {
      name = "steam-xwayland-satellite";
      desktopName = "Steam (XWayland Satellite)";
      comment = "Steam with xwayland-satellite for better Wayland compatibility";
      exec = "steam-xwayland-satellite";
      icon = "steam";
      categories = ["Application" "Game"];
      startupNotify = true;
    })

    # Additional gaming packages
    gamemode
    gamescope
    mangohud # Performance overlay
  ];

  # Ensure Steam has proper gaming optimizations
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;

    # Enable GameScope for better Steam gaming on Wayland
    gamescopeSession.enable = true;
  };

  # Gaming-related environment variables
  environment.sessionVariables = {
    # Force Steam to use XWayland
    STEAM_FORCE_DESKTOPUI_SCALING = "1";

    # Better gaming performance
    __GL_SYNC_TO_VBLANK = "0";
    __GL_THREADED_OPTIMIZATIONS = "1";

    # AMD GPU optimizations (since you have AMD Radeon)
    AMD_VULKAN_ICD = "RADV";
    VK_ICD_FILENAMES = "/run/opengl-driver/share/vulkan/icd.d/radeon_icd.x86_64.json";
  };

  # Enable gamemode for better gaming performance
  programs.gamemode.enable = true;
}
