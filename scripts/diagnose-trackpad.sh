#!/usr/bin/env bash
# Trackpad Diagnostic Script for Little-Rascal
# This script helps diagnose trackpad issues on NixOS

echo "=== Little-Rascal Trackpad Diagnostics ==="
echo "Date: $(date)"
echo

echo "1. Checking for input devices..."
if command -v libinput >/dev/null 2>&1; then
    echo "   ✓ libinput is available"
    echo "   Input devices detected by libinput:"
    sudo libinput list-devices | grep -E "(Device:|Capabilities:)" | head -20
else
    echo "   ✗ libinput not found - this could be the problem!"
fi
echo

echo "2. Checking systemd services..."
if systemctl is-active --quiet systemd-logind; then
    echo "   ✓ systemd-logind is running"
else
    echo "   ✗ systemd-logind is not running"
fi
echo

echo "3. Checking for trackpad hardware..."
if ls /dev/input/mouse* >/dev/null 2>&1; then
    echo "   ✓ Mouse devices found:"
    ls -la /dev/input/mouse*
else
    echo "   ⚠ No mouse devices found in /dev/input/"
fi

if ls /dev/input/event* >/dev/null 2>&1; then
    echo "   ✓ Event devices found:"
    ls -la /dev/input/event* | wc -l
    echo "     Total event devices: $(ls /dev/input/event* | wc -l)"
else
    echo "   ✗ No event devices found"
fi
echo

echo "4. Checking kernel modules..."
modules=("i2c_hid" "hid_multitouch" "psmouse")
for module in "${modules[@]}"; do
    if lsmod | grep -q "$module"; then
        echo "   ✓ $module module is loaded"
    else
        echo "   ⚠ $module module not loaded (may not be needed)"
    fi
done
echo

echo "5. Checking for specific laptop touchpad info..."
if command -v dmesg >/dev/null 2>&1; then
    echo "   Recent touchpad-related kernel messages:"
    dmesg | grep -i -E "(touchpad|synaptics|elan|input)" | tail -5
fi
echo

echo "6. User permissions..."
echo "   Current user: $(whoami)"
echo "   User groups: $(groups)"
if groups | grep -q input; then
    echo "   ✓ User is in 'input' group"
else
    echo "   ⚠ User not in 'input' group (usually not required on NixOS)"
fi
echo

echo "=== Recommendations ==="
echo "If trackpad still doesn't work after enabling libinput:"
echo "1. Reboot the system to ensure all changes take effect"
echo "2. Try: sudo libinput debug-events (to see if events are being detected)"
echo "3. Check dmesg for hardware detection issues"
echo "4. Consider adding specific kernel modules if hardware isn't detected"
echo

echo "=== Configuration Status ==="
if grep -q "services.libinput.enable.*true" /etc/nixos/configuration.nix 2>/dev/null; then
    echo "   ✓ libinput appears to be enabled in configuration"
else
    echo "   ⚠ libinput may not be enabled - check your NixOS configuration"
fi
