# Common Emacs Configuration
# Shared Emacs setup for all machines
{
  config,
  pkgs,
  ...
}: {
  # System-wide Emacs installation
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
