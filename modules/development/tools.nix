{
  config,
  pkgs,
  unstable,
  ...
}: {
  # Development editors and tools
  environment.systemPackages = with pkgs; [
    # Editors
    unstable.zed-editor
    unstable.neovim
    unstable.vscode

    # Language servers
    nixd
    alejandra
    python3Packages.python-lsp-server
    luajitPackages.lua-lsp
    nodePackages.bash-language-server
    vimPlugins.cmp-nvim-lsp
    ccls
    marksman

    # Programming languages and tools
    guile
    gdb

    # Development utilities
    git
    nix-direnv
    direnv
    gh
    github-copilot-cli
    deploy-rs
  ];

  # Note: Emacs is now configured via modules/development/emacs.nix
  # with machine-specific profiles

  # Enable ZSH system-wide for development
  programs.zsh.enable = true;
}
