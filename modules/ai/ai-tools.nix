{
  pkgs,
  unstable,
  ...
}: {
  environment.systemPackages = with pkgs; [
    unstable.gemini-cli
    unstable.code-cursor
    unstable.goose-cli
  ];
}
