{
  pkgs,
  unstable,
  ...
}: {
  environment.systemPackages = with pkgs; [
    unstable.gemini-cli
    goose-cli
    unstable.opencode
    unstable.qwen-code
  ];
}
