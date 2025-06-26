{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    pkgs.unstable.claude-code
  ];
}
