# Example configuration for enabling lab auto-update service
# Add this to your machine's configuration.nix

{
  # Import the lab auto-update service module
  imports = [
    ../services/lab-auto-update.nix
  ];

  # Enable and configure the auto-update service
  services.lab-auto-update = {
    enable = true;
    
    # Schedule updates at 2:00 AM with up to 30 minute random delay
    schedule = "02:00";
    randomizedDelay = "30m";
    
    # Path to your home lab flake
    flakePath = "/home/geir/Projects/home-lab";
    
    # Keep logs for 30 days
    logRetentionDays = 30;
    
    # Persist timer across reboots
    persistent = true;
  };
  
  # Optional: Enable the lab tool package system-wide
  environment.systemPackages = with pkgs; [
    (pkgs.callPackage ../../packages/lab-tools.nix {}).default
  ];
  
  # Optional: Staggered scheduling for different machine types
  # Uncomment and modify based on machine role:
  
  # For database/storage servers (run first)
  # services.lab-auto-update.schedule = "02:00";
  
  # For application servers (run after storage)
  # services.lab-auto-update.schedule = "02:30";
  
  # For development machines (run last)
  # services.lab-auto-update.schedule = "03:00";
}