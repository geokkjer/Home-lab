#!/usr/bin/env bash

# Audio Configuration Validation Script
# This script helps validate that PipeWire with noise suppression is working correctly

set -euo pipefail

echo "🎵 PipeWire Audio Configuration Validator"
echo "========================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

success() {
    echo -e "${GREEN}✅ $1${NC}"
}

warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

error() {
    echo -e "${RED}❌ $1${NC}"
}

info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

# Check if PipeWire is running
echo "1. Checking PipeWire service status..."
if systemctl --user is-active pipewire >/dev/null 2>&1; then
    success "PipeWire service is running"
else
    error "PipeWire service is not running"
    echo "   Try: systemctl --user start pipewire"
    exit 1
fi

# Check WirePlumber
echo ""
echo "2. Checking WirePlumber session manager..."
if systemctl --user is-active wireplumber >/dev/null 2>&1; then
    success "WirePlumber is running"
else
    warning "WirePlumber is not running"
    echo "   Try: systemctl --user start wireplumber"
fi

# Check PipeWire-Pulse
echo ""
echo "3. Checking PipeWire-Pulse compatibility..."
if systemctl --user is-active pipewire-pulse >/dev/null 2>&1; then
    success "PipeWire-Pulse is running"
else
    warning "PipeWire-Pulse is not running"
    echo "   Try: systemctl --user start pipewire-pulse"
fi

# Check for RNNoise plugin
echo ""
echo "4. Checking for RNNoise noise suppression plugin..."
if find /nix/store -name "librnnoise_ladspa.so" 2>/dev/null | head -1 | grep -q .; then
    success "RNNoise plugin found"
    RNNOISE_PATH=$(find /nix/store -name "librnnoise_ladspa.so" 2>/dev/null | head -1)
    info "Located at: $RNNOISE_PATH"
else
    error "RNNoise plugin not found"
    echo "   This might indicate the package is not installed correctly"
fi

# Check audio devices
echo ""
echo "5. Checking available audio devices..."
if command -v wpctl >/dev/null 2>&1; then
    SOURCES=$(wpctl status | grep -A 10 "Audio Sources" | grep -c "│" || echo "0")
    SINKS=$(wpctl status | grep -A 10 "Audio Sinks" | grep -c "│" || echo "0")
    
    if [ "$SOURCES" -gt 0 ]; then
        success "Found $SOURCES audio source(s)"
    else
        warning "No audio sources found"
    fi
    
    if [ "$SINKS" -gt 0 ]; then
        success "Found $SINKS audio sink(s)"
    else
        warning "No audio sinks found"
    fi
else
    error "wpctl command not found"
fi

# Check for GUI applications
echo ""
echo "6. Checking GUI audio applications..."

if command -v easyeffects >/dev/null 2>&1; then
    success "EasyEffects available"
else
    warning "EasyEffects not found"
fi

if command -v pavucontrol >/dev/null 2>&1; then
    success "PulseAudio Volume Control available"
else
    warning "pavucontrol not found"
fi

if command -v helvum >/dev/null 2>&1; then
    success "Helvum patchbay available"
else
    warning "Helvum not found"
fi

# Check configuration files
echo ""
echo "7. Checking configuration files..."

CONFIG_FILES=(
    "/etc/pipewire/pipewire.conf.d/10-noise-suppression.conf"
    "/etc/wireplumber/wireplumber.conf.d/51-noise-suppression.conf"
    "/etc/pipewire/pipewire-pulse.conf.d/10-audio-quality.conf"
)

for config in "${CONFIG_FILES[@]}"; do
    if [ -f "$config" ]; then
        success "$(basename "$config") exists"
    else
        warning "$(basename "$config") not found"
    fi
done

# Check for noise canceling source
echo ""
echo "8. Checking for noise canceling source..."
if command -v pw-dump >/dev/null 2>&1; then
    if pw-dump | jq -r '.[] | select(.info.props."node.name" == "rnnoise_source") | .info.props."node.description"' 2>/dev/null | grep -q "Noise Canceling"; then
        success "Noise Canceling Source device found"
    else
        warning "Noise Canceling Source device not found"
        info "This is normal if no microphone is connected"
    fi
else
    warning "pw-dump not available, cannot check for noise canceling source"
fi

# Performance check
echo ""
echo "9. Checking audio performance..."
if command -v pw-top >/dev/null 2>&1; then
    info "You can monitor real-time performance with: pw-top"
else
    warning "pw-top not available for performance monitoring"
fi

# Summary
echo ""
echo "🎯 Quick Start Commands:"
echo "========================"
echo ""
echo "Start audio setup wizard:           audio-setup"
echo "Test microphone:                    microphone-test" 
echo "Launch EasyEffects:                 easyeffects"
echo "Control volumes:                    pavucontrol"
echo "Audio routing:                      helvum"
echo "Monitor performance:                pw-top"
echo "Device status:                      wpctl status"
echo ""

# Check if user wants to run a test
echo "Would you like to run a quick microphone test? (y/N)"
read -r response
if [[ "$response" =~ ^[Yy]$ ]]; then
    echo ""
    info "Starting microphone test..."
    if command -v microphone-test >/dev/null 2>&1; then
        microphone-test
    else
        echo "Recording 3 seconds of audio..."
        if command -v arecord >/dev/null 2>&1 && command -v aplay >/dev/null 2>&1; then
            arecord -d 3 -f cd /tmp/audio_test.wav 2>/dev/null && \
            echo "Playing back..." && \
            aplay /tmp/audio_test.wav 2>/dev/null && \
            rm -f /tmp/audio_test.wav
            success "Microphone test completed"
        else
            error "Audio testing tools not available"
        fi
    fi
fi

echo ""
success "Audio configuration validation completed!"
echo ""
info "If you encounter issues, try:"
echo "  • systemctl --user restart pipewire pipewire-pulse wireplumber"
echo "  • Check the README.md for detailed troubleshooting"
echo "  • Run 'audio-setup' for interactive configuration"
