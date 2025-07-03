{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.emacs-profiles;

  # Emacs package configurations for different profiles
  packageSets = {
    # Essential packages for all profiles
    essential = epkgs:
      with epkgs; [
        use-package
        diminish
        bind-key
        which-key
        exec-path-from-shell # Critical for integrating with Nix environment
      ];

    # Minimal packages for server profile
    minimal = epkgs:
      with epkgs; [
        # Basic editing
        smartparens
        expand-region

        # Essential navigation (pure Emacs, no external deps)
        vertico
        consult
        marginalia
        orderless

        # Basic modes for config files
        nix-mode # Essential for Nix ecosystem
        yaml-mode
        markdown-mode

        # Org mode essentials
        org
        org-roam
      ];

    # Development packages for laptop/workstation
    development = epkgs:
      with epkgs; [
        # Advanced navigation and completion
        vertico
        consult
        marginalia
        orderless
        embark
        embark-consult
        corfu
        cape

        # Project management
        projectile
        magit
        forge

        # Development tools
        lsp-mode
        lsp-ui
        company
        flycheck
        yasnippet

        # Language support
        nix-mode
        rust-mode
        python-mode
        typescript-mode
        json-mode
        yaml-mode
        markdown-mode
        lsp-scheme

        # Org mode and knowledge management
        org
        org-roam
        org-roam-ui

        # UI enhancements
        highlight-defined
        doom-themes
        doom-modeline
        all-the-icons
        rainbow-delimiters
        highlight-indent-guides

        # Editing enhancements
        elisp-slime-nav
        aggressive-indent
        smartparens
        expand-region
        multiple-cursors
        avy
        ace-window

        # Terminal integration
        vterm
        eshell-git-prompt
      ];

    # Full workstation packages
    workstation = epkgs:
      with epkgs; [
        # All development packages plus extras
        pdf-tools
        nov # EPUB reader
        elfeed # RSS reader
        mu4e # Email (if configured)
        dired-sidebar
        treemacs
        treemacs-projectile
        treemacs-magit
      ];
  };

  # Generate Emacs configuration based on profile
  # Uses emacs-pgtk for native Wayland support on desktop profiles
  # Uses emacs-nox for server profiles (no X11/GUI dependencies)
  emacsWithProfile = profile: let
    # Choose Emacs package based on profile
    emacsPackage =
      if profile == "nox"
      then pkgs.emacs-nox # Terminal only
      else pkgs.emacs-pgtk; # Pure GTK for native Wayland support

    # Combine package sets based on profile
    selectedPackages = epkgs:
      (packageSets.essential epkgs)
      ++ (
        if profile == "nox"
        then packageSets.minimal epkgs
        else (packageSets.development epkgs) ++ (packageSets.workstation epkgs)
      );
  in
    emacsPackage.pkgs.withPackages (epkgs: selectedPackages epkgs);
in {
  options.services.emacs-profiles = {
    enable = mkEnableOption "Emacs with machine-specific profiles";

    profile = mkOption {
      type = types.enum ["gui" "nox"];
      default = "gui";
      description = "Emacs profile: gui (with UI) or nox (terminal only)";
    };

    enableDaemon = mkOption {
      type = types.bool;
      default = true;
      description = "Enable Emacs daemon service";
    };

    user = mkOption {
      type = types.str;
      default = "geir";
      description = "User to run Emacs daemon for";
    };
  };

  config = mkIf cfg.enable {
    # Install Emacs with the selected profile
    environment.systemPackages = [
      (emacsWithProfile cfg.profile)
      pkgs.silver-searcher
    ];

    # System-wide Emacs daemon (optional)
    services.emacs = mkIf cfg.enableDaemon {
      enable = true;
      package = emacsWithProfile cfg.profile;
    };

    # Create the Emacs configuration directory structure
    environment.etc = {
      "emacs/init.el" = {
        source = ../../dotfiles/geir/emacs-config/init-nix.el;
        mode = "0644";
      };

      # Module files
      "emacs/modules/ui.el" = {
        source = ../../dotfiles/geir/emacs-config/modules/ui.el;
        mode = "0644";
      };

      "emacs/modules/completion.el" = {
        source = ../../dotfiles/geir/emacs-config/modules/completion.el;
        mode = "0644";
      };

      "emacs/modules/navigation.el" = {
        source = ../../dotfiles/geir/emacs-config/modules/navigation.el;
        mode = "0644";
      };

      "emacs/modules/development.el" = {
        source = ../../dotfiles/geir/emacs-config/modules/development.el;
        mode = "0644";
      };

      "emacs/modules/elisp-development.el" = {
        source = ../../dotfiles/geir/emacs-config/modules/elisp-development.el;
        mode = "0644";
      };

      "emacs/modules/claude-code.el" = mkIf (cfg.profile == "gui") {
        source = ../../dotfiles/geir/emacs-config/modules/claude-code.el;
        mode = "0644";
      };
    };

    # Environment variables for Nix integration
    environment.variables = {
      EMACS_PROFILE = cfg.profile;
      RG_PATH = "${pkgs.ripgrep}/bin/rg";
      FD_PATH = "${pkgs.fd}/bin/fd";
      SQLITE_PATH = "${pkgs.sqlite}/bin/sqlite3";
      AG_PATH = "${pkgs.silver-searcher}/bin/ag";

      # Language servers
      NIL_LSP_PATH = "${pkgs.nixd}/bin/nixd";
      BASH_LSP_PATH = "${pkgs.nodePackages.bash-language-server}/bin/bash-language-server";
      YAML_LSP_PATH = "${pkgs.nodePackages.yaml-language-server}/bin/yaml-language-server";

      # Formatters
      SHELLCHECK_PATH = "${pkgs.shellcheck}/bin/shellcheck";
      ALEJANDRA_PATH = "${pkgs.alejandra}/bin/alejandra";
    };

    # Ensure the user can access the Emacs daemon
    systemd.user.services.emacs = mkIf cfg.enableDaemon {
      environment = {
        EMACS_PROFILE = cfg.profile;
        NIX_PATH = config.environment.variables.NIX_PATH or "";
      };
    };
  };
}
