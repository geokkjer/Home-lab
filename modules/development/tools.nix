{ config, pkgs, ... }: {
  # Development editors and tools
  environment.systemPackages = with pkgs; [
    # Editors
    zed-editor
    neovim
    emacs
    vscode
    vscodium-fhs

    # Language servers
    nixd
    zls
    alejandra
    python3Packages.python-lsp-server
    gopls
    luajitPackages.lua-lsp
    nodePackages.bash-language-server
    vimPlugins.cmp-nvim-lsp
    ccls
    marksman

    # Programming languages and tools
    guile
    rustup
    gdb

    # Development utilities
    git
    nix-direnv
    gh
    github-copilot-cli
  ];

  # System-wide Emacs daemon
  services.emacs.enable = true;

  # Enable ZSH system-wide for development
  programs.zsh.enable = true;
}