# TODO install the latest version of Aider Chat
{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    aider-chat-full
  ];
}
