# SSH Key Management Module
# Two-key strategy: admin (sma) and development (geir)
{
  config,
  pkgs,
  lib,
  ...
}: {
  # Firewall configuration for SSH
  networking.firewall.allowedTCPPorts = [22];

  # Global SSH daemon configuration
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
      PubkeyAuthentication = true;
    };

    # Use modern, secure algorithms only
    extraConfig = ''
      PubkeyAcceptedKeyTypes ssh-ed25519,ssh-ed25519-cert-v01@openssh.com
      KexAlgorithms curve25519-sha256@libssh.org
      Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com
      MACs hmac-sha2-256-etm@openssh.com,hmac-sha2-512-etm@openssh.com
    '';
  };

  # SSH client configuration
  programs.ssh = {
    startAgent = true;
    extraConfig = ''
      # Default to development key for daily use
      Host *
        IdentityFile ~/.ssh/id_ed25519_dev
        AddKeysToAgent yes
        ServerAliveInterval 60
        ServerAliveCountMax 3

      # Admin access to servers (use sma user)
      Host admin-* *.admin
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      # Git services (use geir user with dev key)
      Host git.* github.com gitlab.com
        User git
        IdentityFile ~/.ssh/id_ed25519

      # Home lab servers (geir user for development access)
      Host sleeper-service sleeper-service.home 10.0.0.8
        User geir
        IdentityFile /home/geir/.ssh/id_ed25519_dev

      Host grey-area grey-area.home 10.0.0.12
        User geir
        IdentityFile /home/geir/.ssh/id_ed25519_dev

      Host reverse-proxy reverse-proxy.home 46.226.104.98
        User geir
        IdentityFile /home/geir/.ssh/id_ed25519_dev

      # Admin access to servers (when needed)
      Host admin-sleeper sleeper-service.admin
        Hostname 10.0.0.8
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      Host admin-grey grey-area.admin
        Hostname 10.0.0.12
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      Host admin-reverse reverse-proxy.admin
        Hostname 46.226.104.98
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      # Direct sma user access via Tailscale for deployments
      Host sma@sleeper-service.tail807ea.ts.net
        Hostname sleeper-service.tail807ea.ts.net
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      Host sma@grey-area.tail807ea.ts.net
        Hostname grey-area.tail807ea.ts.net
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      Host sma@reverse-proxy.tail807ea.ts.net
        Hostname reverse-proxy.tail807ea.ts.net
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      Host sma@little-rascal.tail807ea.ts.net
        Hostname little-rascal.tail807ea.ts.net
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      # Localhost sma user for local deployment from laptop
      Host sma@localhost
        Hostname localhost
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin

      # Tailscale network
      Host 100.* *.tail*
        User geir
        IdentityFile /home/geir/.ssh/id_ed25519_dev
    '';
  };
}
