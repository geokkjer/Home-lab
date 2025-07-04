{ config, pkgs, ... }:
let
  # Fix cowsql build issue with glibc 2.40
  cowsql-fixed = pkgs.cowsql.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [
      (pkgs.writeText "cowsql-float-t-fix.patch" ''
        --- a/src/lib/serialize.h
        +++ b/src/lib/serialize.h
        @@ -37,7 +37,9 @@
         #define SERIALIZE_H_
         
         #include <uv.h>
        +#ifndef _MATH_H
         typedef double float_t;
        +#endif
         
         struct serialize;
      '')
    ];
  });
  
  incus-fixed = pkgs.incus.override {
    cowsql = cowsql-fixed;
  };
in
{
  virtualisation.incus = {
    enable = true;
    ui.enable = true;
    package = incus-fixed;
  };

  environment.systemPackages = [
    incus-fixed
    pkgs.lxc
  ];
  
  networking.firewall.allowedTCPPorts = [ 8443 ];
}
