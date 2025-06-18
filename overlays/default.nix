final: prev: {
  # Custom packages and overrides for Home-lab infrastructure

  # Home-lab specific tools and scripts

  # Override packages with custom configurations
  starship = prev.starship.override {
    # Add any custom starship configuration
  };

  # Add unstable packages to stable
  inherit
    (final.unstable or {})
    # Example: latest version of development tools
    # zed-editor
    # github-copilot-cli
    ;

  # Custom vim/neovim configurations
  vim-home-lab = prev.vim.override {
    features = "huge";
  };

  # Emacs with custom packages
  emacs-home-lab = prev.emacs.override {
    # Custom emacs configuration
  };
}
