{ config, pkgs, ... }: {
  # Font configuration
  fonts.packages = with pkgs; [
    # Base fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    dina-font
    proggyfonts
    
    # GitHub fonts
    mona-sans
    hubot-sans
    inter-nerdfont

    # Nerd Fonts (updated syntax for NixOS 25.05)
    nerd-fonts.meslo-lg
    nerd-fonts.jetbrains-mono
    nerd-fonts.fira-code
    nerd-fonts.droid-sans-mono
    nerd-fonts.hack
    nerd-fonts.iosevka
    nerd-fonts.iosevka-term
  ];

  # Console configuration
  console = {
    font = "Lat2-Terminus16";
    keyMap = "no";
  };

  # Internationalization
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Oslo";
}