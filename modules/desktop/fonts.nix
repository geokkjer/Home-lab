{
  config,
  pkgs,
  ...
}: {
  # Minimal font configuration - users can add more fonts as needed
  fonts.packages = with pkgs; [
    # Essential system fonts
    noto-fonts
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    inter-nerdfont

    # Nerd fonts
    nerd-fonts.iosevka-term
    nerd-fonts.meslo-lg
    nerd-fonts.jetbrains-mono
    nerd-fonts.zed-mono
    nerd-fonts.monaspace
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
