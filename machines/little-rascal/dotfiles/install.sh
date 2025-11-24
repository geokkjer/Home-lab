#!/usr/bin/env bash
# Install script for little-rascal (Arch Linux laptop) dotfiles
# Simple bash script to link dotfiles to home directory

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOME_DIR="$HOME"
BACKUP_DIR="$HOME_DIR/.dotfiles-backup-$(date +%Y%m%d-%H%M%S)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
  echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $1"
}

# Create symlink safely
link_file() {
  local src="$1"
  local dst="$2"
  
  # Create parent directory if needed
  mkdir -p "$(dirname "$dst")"
  
  # Handle existing file
  if [[ -e "$dst" ]] || [[ -L "$dst" ]]; then
    if [[ -L "$dst" ]] && [[ "$(readlink "$dst")" == "$src" ]]; then
      log_info "Already linked: $dst"
      return 0
    fi
    
    log_warn "Backing up existing file: $dst"
    mkdir -p "$BACKUP_DIR"
    mv "$dst" "$BACKUP_DIR/$(basename "$dst")"
  fi
  
  ln -s "$src" "$dst"
  log_info "Linked: $dst → $src"
}

main() {
  log_info "Installing dotfiles from $SCRIPT_DIR"
  
  # Install shell configuration
  log_info "Setting up shell configuration..."
  link_file "$SCRIPT_DIR/.zshrc" "$HOME_DIR/.zshrc"
  
  # Install starship configuration
  log_info "Setting up starship configuration..."
  link_file "$SCRIPT_DIR/.config/starship.toml" "$HOME_DIR/.config/starship.toml"
  
  # Add more dotfiles as needed
  # link_file "$SCRIPT_DIR/.gitconfig" "$HOME_DIR/.gitconfig"
  # link_file "$SCRIPT_DIR/.config/nvim" "$HOME_DIR/.config/nvim"
  
  log_info "Installation complete!"
  
  if [[ -d "$BACKUP_DIR" ]]; then
    log_warn "Backed up files to: $BACKUP_DIR"
  fi
}

main "$@"
