{
  pkgs,
  config,
  ...
}: {
  # Create the git user before Forgejo tries to use it
  users.users.git = {
    isSystemUser = true;
    group = "git";
    shell = pkgs.bash;
    home = "/var/lib/forgejo";
    createHome = true;
    description = "Forgejo Git Service";
  };

  users.groups.git = {};

  services.forgejo = {
    enable = true;
    package = pkgs.forgejo;
    user = "git"; # Use the git user we created above
    group = "git";
  };

  services.forgejo.settings = {
    DEFAULT = {
      RUN_MODE = "prod";
    };
    service = {
      DISABLE_REGISTRATION = true;
    };
    server = {
      ROOT_URL = "https://git.geokkjer.eu";
      SSH_DOMAIN = "git.geokkjer.eu";
      SSH_PORT = 2222;
      # Use system SSH server instead of built-in
      DISABLE_SSH = false;
      START_SSH_SERVER = false;
      # Configure SSH user
      SSH_USER = "git";
    };
    repository = {
      ENABLE_PUSH_CREATE_USER = true;
    };
    other = {
      SHOW_FOOTER_VERSION = true;
    };
  };
}
