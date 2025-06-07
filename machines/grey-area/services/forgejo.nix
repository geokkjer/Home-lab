{ pkgs, config, ... }:
{
  services.forgejo = {
    enable = true;
    # Use the default 'forgejo' user, not 'git'
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
      SSH_PORT = 1337;
      # Disable built-in SSH server, use system SSH instead
      DISABLE_SSH = false;
      START_SSH_SERVER = false;
    };
    repository = {
      ENABLE_PUSH_CREATE_USER = true;
    };
    other = {
      SHOW_FOOTER_VERSION = true;
    };
  };
}
