# Shared Shell Aliases Module
# Common shell aliases for all users in the Home Lab infrastructure
{
  config,
  pkgs,
  ...
}: {
  programs.zsh = {
    # Common aliases for all users
    shellAliases = {
      # === File System Navigation & Management ===
      "ll" = "eza -l --color=auto --group-directories-first";
      "la" = "eza -la --color=auto --group-directories-first";
      "tree" = "eza --tree";
      
      # Safety first
      "rm" = "rm -i";
      "mv" = "mv -i";
      "cp" = "cp -i";

      # === System Management ===
      "top" = "btop";
      "disk-usage" = "df -h";
      "mem-usage" = "free -h";
      "processes" = "ps aux | head -20";

      # === NixOS Management ===
      "rebuild" = "sudo nixos-rebuild switch";
      "rebuild-test" = "sudo nixos-rebuild test";
      "rebuild-boot" = "sudo nixos-rebuild boot";
      "collect" = "sudo nix-collect-garbage -d";
      "optimise" = "sudo nix-store --optimise";

      # === Git Shortcuts ===
      "gs" = "git status";
      "ga" = "git add";
      "gc" = "git commit";
      "gp" = "git push";
      "gl" = "git log --oneline";
      "gd" = "git diff";

      # === Container Management ===
      "pdm" = "podman";
      "pdc" = "podman-compose";
      "pods" = "podman ps -a";
      "images" = "podman images";
      "logs" = "podman logs";

      # === Network Utilities ===
      "ping" = "ping -c 5";
      "myip" = "curl -s ifconfig.me";
      "ports" = "ss -tulpn";
      "connections" = "ss -tuln";

      # === Media & Downloads ===
      "youtube-dl" = "yt-dlp";

      # === Security & Auditing ===
      "audit-users" = "cat /etc/passwd | grep -E '/bin/(bash|zsh|fish)'";
      "audit-sudo" = "cat /etc/sudoers.d/*";
    };
  };
}
