{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    tldr
    eza
    bat
    ripgrep
    du-dust
    bottom
    fd
    fzf
    zoxide
    uutils-coreutils-noprefix
    direnv       # Directory-based environment management
    nix-direnv   # Nix integration for direnv
  ];
  environment.shellAliases = {
    vi = "nvim";
    vim = "nvim";
    h = "tldr";
    # oxidized
    ls = "eza -l";
    cat = "bat";
    grep = "rg";
    top = "btm";
    du = "dust";
    find = "fd";
  };
}