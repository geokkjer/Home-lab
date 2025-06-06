{ config, pkgs, ... }:
{
  services.jellyfin = { 
  	enable = true;
	  group = "users";
	};	
  networking.firewall.allowedTCPPorts = [ 8096 8920 ];
  networking.firewall.allowedUDPPorts = [ 1900 7359 ];
}
