{config, pkgs, ... }:
  {
    environment.systemPackages = with pkgs; [
      tailscale
    ];

    services.tailscale.enable = true;
    networking.firewall = {
      # trace: warning: Strict reverse path filtering breaks Tailscale exit node
      # use and some subnet routing setups. Consider setting
      # `networking.firewall.checkReversePath` = 'loose'
      checkReversePath = "loose";
      trustedInterfaces = [ "tailscale0" ];
    };
  }
#+end80808080 * nginx

#+begin_src nix
