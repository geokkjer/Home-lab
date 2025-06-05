#!/usr/bin/env bash
# SSH Key Generation Script - Two-Key Strategy
# Generates admin and development SSH keys

set -euo pipefail

SSH_DIR="$HOME/.ssh"
HOSTNAME="$(hostname)"
EMAIL_BASE="geir@geokkjer.eu"

echo "🔑 Setting up SSH keys for $HOSTNAME"
echo "📧 Using email base: $EMAIL_BASE"

# Create SSH directory if it doesn't exist
mkdir -p "$SSH_DIR"
chmod 700 "$SSH_DIR"

# Backup existing key if it exists
if [[ -f "$SSH_DIR/id_ed25519" ]]; then
    echo "💾 Backing up existing key to id_ed25519.backup"
    cp "$SSH_DIR/id_ed25519" "$SSH_DIR/id_ed25519.backup"
    cp "$SSH_DIR/id_ed25519.pub" "$SSH_DIR/id_ed25519.pub.backup"
fi

echo ""
echo "🔐 Generating two SSH keys:"
echo "  1. Admin key (for sma user, server administration)"
echo "  2. Development key (for geir user, git, daily use)"
echo ""

# Generate admin key
ADMIN_KEY="$SSH_DIR/id_ed25519_admin"
if [[ ! -f "$ADMIN_KEY" ]]; then
    echo "🔐 Generating admin key..."
    ssh-keygen -t ed25519 -f "$ADMIN_KEY" -C "$EMAIL_BASE-admin" -N ""
    chmod 600 "$ADMIN_KEY"
    chmod 644 "$ADMIN_KEY.pub"
    echo "✅ Generated: $ADMIN_KEY"
else
    echo "⏭️  Admin key already exists"
fi

# Generate development key  
DEV_KEY="$SSH_DIR/id_ed25519_dev"
if [[ ! -f "$DEV_KEY" ]]; then
    echo "🔐 Generating development key..."
    ssh-keygen -t ed25519 -f "$DEV_KEY" -C "$EMAIL_BASE-dev" -N ""
    chmod 600 "$DEV_KEY"
    chmod 644 "$DEV_KEY.pub"
    echo "✅ Generated: $DEV_KEY"
else
    echo "⏭️  Development key already exists"
fi

echo ""
echo "🎯 Next steps:"
echo "1. Add these public keys to your NixOS configuration:"
echo "2. Deploy updated configuration to target servers"
echo "3. Test SSH access with both keys"
echo "4. Update external Git services with new development key"
echo ""

echo "📋 Public keys to add to NixOS configuration:"
echo ""

if [[ -f "$ADMIN_KEY.pub" ]]; then
    echo "# Admin key (add to security.ssh-keys.admin in modules/security/ssh-keys.nix)"
    echo "\"$(cat "$ADMIN_KEY.pub")\""
    echo ""
fi

if [[ -f "$DEV_KEY.pub" ]]; then
    echo "# Development key (add to security.ssh-keys.development in modules/security/ssh-keys.nix)" 
    echo "\"$(cat "$DEV_KEY.pub")\""
    echo ""
fi

echo "💡 Usage examples:"
echo "  ssh geir@sleeper-service.home     # Uses dev key automatically"
echo "  ssh admin-sleeper                 # Uses admin key for sma user"
echo "  git clone git@github.com:user/repo # Uses dev key for git"
echo ""

echo "🔄 To update your Git remotes with the new key:"
echo "  # Add new key to GitHub/GitLab first, then:"
echo "  ssh -T git@github.com  # Test the connection"
echo ""

echo "✅ SSH key setup complete!"
