{ config, pkgs, lib, ... }:
{
  environment.systemPackages = with pkgs;
    [
      kitty kitty-themes termpdfpy
    ];
}
