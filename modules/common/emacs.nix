# Common Emacs Configuration
# Shared Emacs setup for all machines

{
  config,
  pkgs,
  ...
}: {
  # System-wide Emacs configuration
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    defaultEditor = true;
  };

  # Emacs packages and configuration
  environment.systemPackages = with pkgs; [
    emacs
    # Basic Emacs utilities
    emacsPackages.use-package
  ];

  # Set Emacs as default editor
  environment.sessionVariables = {
    EDITOR = "emacs";
    VISUAL = "emacs";
  };
}