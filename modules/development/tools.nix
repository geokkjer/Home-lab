{
  config,
  pkgs,
  ...
}: {
  # Development editors and tools
  environment.systemPackages = with pkgs; [
    # Editors
    zed-editor
    neovim
    vscode

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
    direnv
    gh
    github-copilot-cli
    deploy-rs
    # ai
    claude-code
  ];

  # Note: Emacs is now configured via modules/development/emacs.nix
  # with machine-specific profiles

  # Enable ZSH system-wide for development
  programs.zsh.enable = true;
}
