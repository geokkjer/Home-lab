# Input Configuration Module
# Handles touchpad, keyboard, and other input devices
{
  config,
  lib,
  pkgs,
  ...
}: {
  # Enable libinput for touchpad support
  # This is the recommended touchpad driver for NixOS since 17.09
  services.libinput = {
    enable = true;

    # Touchpad-specific settings
    touchpad = {
      # Enable tap-to-click (can be disabled if unwanted)
      tapping = true;

      # Enable two-finger scrolling
      scrollMethod = "twofinger";

      # Enable natural scrolling (macOS-style, disable if you prefer traditional)
      naturalScrolling = false;

      # Disable touchpad while typing to prevent accidental input
      disableWhileTyping = true;

      # Middle button emulation (three-finger tap)
      middleEmulation = true;
    };

    # Mouse settings (for external mice)
    mouse = {
      # Standard settings for mice
      naturalScrolling = false;
      accelProfile = "adaptive";
    };
  };

  # Additional input packages that might be useful
  environment.systemPackages = with pkgs; [
    # Input device utilities
    libinput-gestures # For custom touchpad gestures
    evtest # For testing input devices
    xinput # X11 input device utility (still useful in Wayland)
  ];

  # Enable support for additional input methods if needed
  # (This is separate from touchpad functionality)
  i18n.inputMethod = {
    enable = false; # Set to true if you need input methods for other languages
    # type = "ibus";  # Uncomment and configure if needed
  };
}
