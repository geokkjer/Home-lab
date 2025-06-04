{ pkgs, ... }:

{
  users.users.geir = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel"  ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      # Browsers
      chromium
      vivaldi
      vivaldi-ffmpeg-codecs
      nyxt
      firefox

      # Shell & tools
      zsh
      zsh-completions
      nix-zsh-completions
      starship
      nix-direnv

      # Audio & system
      ncpamixer
      fastfetch
      hyfetch
      nerdfetch
      emacsPackages.vterm
      virt-manager
      pavucontrol
      gnome-tweaks
      beauty-line-icon-theme

      # Fun & misc
      neo-cowsay
      fortune
      clolcat
      zellij
      gimp
      vesktop
      koodo-reader
      # Github CLI
      gh
    ];
  };

  environment.systemPackages = with pkgs;
    [
      zsh
      zsh-completions
      nix-zsh-completions
      zsh-autocomplete
      zsh-autosuggestions
      zsh-syntax-highlighting
    ];

  programs.zsh.enable = true;
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.autosuggestions = {
    enable = true;
    historySearch = true;
  };
  programs.zsh.history = {
    enable = true;
    shareHistory = true;
    saveOnExit = true;
  };
}
