# SSH Key Management Module
# Two-key strategy: admin (sma) and development (geir)
{ config, pkgs, lib, ... }:

{
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

  # Centralized SSH key management
  security.ssh-keys = {
    # Admin keys for sma user (server administration)
    admin = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPgzKS1N7+7+N1/8U8++1pl4hapDm6TOy0QhrfrYA8mz geir@geokkjer.eu-admin"
    ];
    
    # Development keys for geir user (git, daily use)
    development = [
      # Current key (keep for continuity during transition)
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHeOvTHIw+hZOAiWkIrz9t11UeGwxAMx7jN/1IIdgq7O geokkjer@gmail.com"
      # New development key
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHukJK0Kc1YexvzF8PdqaqWNZdVffGoM6ePPMecrU6dM geir@geokkjer.eu-dev"
    ];
  };

  # SSH client configuration
  programs.ssh = {
    enable = true;
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
        IdentityFile ~/.ssh/id_ed25519_dev
        
      # Home lab servers (geir user for development access)
      Host sleeper-service sleeper-service.home 10.0.0.8
        User geir
        IdentityFile ~/.ssh/id_ed25519_dev
        
      Host grey-area grey-area.home 10.0.0.11
        User geir
        IdentityFile ~/.ssh/id_ed25519_dev
        
      Host reverse-proxy reverse-proxy.home 10.0.0.12
        User geir
        IdentityFile ~/.ssh/id_ed25519_dev
        
      # Admin access to servers (when needed)
      Host admin-sleeper sleeper-service.admin
        Hostname 10.0.0.8
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin
        
      Host admin-grey grey-area.admin
        Hostname 10.0.0.11
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin
        
      Host admin-reverse reverse-proxy.admin
        Hostname 10.0.0.12
        User sma
        IdentityFile ~/.ssh/id_ed25519_admin
        
      # Tailscale network
      Host 100.* *.tail*
        User geir
        IdentityFile ~/.ssh/id_ed25519_dev
    '';
  };
}
