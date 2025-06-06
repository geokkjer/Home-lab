{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    tldr
    eza
    bat
    ripgrep
  ];
  environment.shellAliases = {
    vi = "nvim";
    vim = "nvim";
    h = "tldr";
    # oxidized
    ls = "eza -l";
    cat = "bat";
    grep = "rg";
    top = "btm --color gruvbox";
    # some tools
  };
}
