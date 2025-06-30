#!/usr/bin/env bash
# Little Rascal NixOS Installation and Deployment Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
LAPTOP_HOSTNAME="little-rascal"
LAPTOP_IP="little-rascal.tail807ea.ts.net"  # Or use IP address
SSH_USER="geir"
FLAKE_PATH="/home/geir/Home-lab"

echo -e "${BLUE}=== Little Rascal NixOS Deployment Helper ===${NC}"
echo

# Function to print colored output
print_step() {
    echo -e "${GREEN}[STEP]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

# Check if we're running from the Home-lab directory
if [ ! -f "flake.nix" ]; then
    print_error "Please run this script from the Home-lab directory"
    exit 1
fi

# Menu selection
echo "What would you like to do?"
echo "1. Generate hardware configuration (run this on the laptop after minimal install)"
echo "2. Test SSH connection to laptop"
echo "3. Deploy full configuration to laptop"
echo "4. Check deployment status"
echo "5. Show manual installation steps"
echo
read -p "Enter your choice (1-5): " choice

case $choice in
    1)
        print_step "Generating hardware configuration..."
        echo "Run this command ON THE LAPTOP after minimal NixOS installation:"
        echo
        echo -e "${YELLOW}sudo nixos-generate-config --show-hardware-config > hardware-configuration.nix${NC}"
        echo
        echo "Then copy the hardware-configuration.nix to this machine at:"
        echo "  $FLAKE_PATH/machines/little-rascal/hardware-configuration.nix"
        ;;
        
    2)
        print_step "Testing SSH connection to laptop..."
        if ssh -o ConnectTimeout=5 $SSH_USER@$LAPTOP_IP "echo 'SSH connection successful!'" 2>/dev/null; then
            print_info "✅ SSH connection to $LAPTOP_IP successful!"
            
            # Check if Tailscale is running
            if ssh $SSH_USER@$LAPTOP_IP "systemctl is-active tailscale" 2>/dev/null | grep -q "active"; then
                print_info "✅ Tailscale is running on laptop"
            else
                print_warning "⚠️  Tailscale might not be running on laptop"
            fi
        else
            print_error "❌ Cannot connect to $LAPTOP_IP via SSH"
            echo "Make sure:"
            echo "  - The laptop is connected to the network"
            echo "  - SSH is enabled and running"
            echo "  - Your SSH key is properly configured"
            echo "  - Tailscale is connected (if using .ts.net address)"
        fi
        ;;
        
    3)
        print_step "Deploying full configuration to laptop..."
        
        # First, test the configuration builds locally
        print_info "Building configuration locally first..."
        if nixos-rebuild build --flake .#little-rascal; then
            print_info "✅ Configuration builds successfully"
        else
            print_error "❌ Configuration failed to build"
            exit 1
        fi
        
        # Deploy using deploy-rs
        print_info "Deploying to laptop via deploy-rs..."
        if deploy .#little-rascal; then
            print_info "✅ Deployment successful!"
        else
            print_error "❌ Deployment failed"
            print_info "You can try manual deployment with:"
            echo "  nixos-rebuild switch --flake .#little-rascal --target-host $SSH_USER@$LAPTOP_IP --use-remote-sudo"
        fi
        ;;
        
    4)
        print_step "Checking deployment status..."
        print_info "Current system generation on laptop:"
        ssh $SSH_USER@$LAPTOP_IP "sudo nixos-rebuild list-generations | tail -5"
        
        print_info "Current running configuration:"
        ssh $SSH_USER@$LAPTOP_IP "readlink /run/current-system"
        ;;
        
    5)
        print_step "Manual installation steps:"
        echo
        echo -e "${YELLOW}1. Boot from NixOS installer ISO${NC}"
        echo "2. Set up disk partitioning (example for UEFI system):"
        echo "   sudo parted /dev/nvme0n1 mklabel gpt"
        echo "   sudo parted /dev/nvme0n1 mkpart ESP fat32 1MiB 512MiB"
        echo "   sudo parted /dev/nvme0n1 set 1 esp on"
        echo "   sudo parted /dev/nvme0n1 mkpart primary 512MiB 100%"
        echo
        echo "3. Format partitions:"
        echo "   sudo mkfs.fat -F 32 -n BOOT /dev/nvme0n1p1"
        echo "   sudo mkfs.ext4 -L nixos /dev/nvme0n1p2"
        echo
        echo "4. Mount filesystems:"
        echo "   sudo mount /dev/disk/by-label/nixos /mnt"
        echo "   sudo mkdir -p /mnt/boot"
        echo "   sudo mount /dev/disk/by-label/BOOT /mnt/boot"
        echo
        echo "5. Generate and edit configuration:"
        echo "   sudo nixos-generate-config --root /mnt"
        echo "   sudo nano /mnt/etc/nixos/configuration.nix"
        echo "   # Copy minimal-configuration.nix content and adjust disk UUIDs"
        echo
        echo "6. Install NixOS:"
        echo "   sudo nixos-install"
        echo
        echo "7. Set root password and reboot:"
        echo "   sudo nixos-enter --root /mnt"
        echo "   passwd"
        echo "   exit"
        echo "   reboot"
        echo
        echo -e "${YELLOW}8. After reboot, connect to Tailscale and run this script again with option 3${NC}"
        ;;
        
    *)
        print_error "Invalid choice"
        exit 1
        ;;
esac

echo
print_info "Done!"
