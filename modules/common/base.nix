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