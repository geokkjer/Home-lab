#!/usr/bin/env bash

# Voice Distortion Troubleshoot Script
# This script helps diagnose and fix voice distortion issues in PipeWire

# Use safer error handling - don't exit on all errors
set -uo pipefail

echo "🎤 Voice Distortion Troubleshoot Tool"
echo "===================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
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

highlight() {
    echo -e "${CYAN}🔧 $1${NC}"
}

echo "Let's diagnose your voice distortion issue step by step..."
echo ""

# 1. Check current audio settings
echo "1. Current Audio Configuration"
echo "=============================="

if command -v wpctl >/dev/null 2>&1; then
    echo "Default devices:"
    wpctl status | head -20
    echo ""
    
    # Get default source
    DEFAULT_SOURCE=$(wpctl inspect @DEFAULT_AUDIO_SOURCE@ 2>/dev/null | grep "node.name" | head -1 | sed 's/.*"\(.*\)".*/\1/' || echo "unknown")
    info "Current default source: $DEFAULT_SOURCE"
    
    # Check sample rate
    CURRENT_RATE=$(pw-metadata -n settings | grep "clock.rate" | awk '{print $3}' || echo "unknown")
    info "Current sample rate: $CURRENT_RATE Hz"
    
    # Check buffer size
    CURRENT_QUANTUM=$(pw-metadata -n settings | grep "clock.quantum" | awk '{print $3}' || echo "unknown")
    info "Current buffer size: $CURRENT_QUANTUM samples"
    
else
    error "wpctl not available"
fi

echo ""

# 2. Check for common distortion causes
echo "2. Distortion Diagnosis"
echo "======================"

# Check if using RNNoise filter
if command -v pw-dump >/dev/null 2>&1 && command -v jq >/dev/null 2>&1; then
    if pw-dump 2>/dev/null | jq -r '.[] | select(.info.props."node.name" == "rnnoise_source")' 2>/dev/null | grep -q "rnnoise" 2>/dev/null; then
        warning "You're using the RNNoise filter chain - this might be causing distortion"
        echo "   The automatic filter chain can sometimes cause artifacts"
    else
        info "Not using automatic RNNoise filter"
    fi
else
    warning "Cannot check RNNoise filter status (pw-dump or jq not available)"
fi

# Check for high CPU usage
if command -v pw-top >/dev/null 2>&1; then
    highlight "Checking PipeWire performance (5 seconds)..."
    if timeout 5 pw-top --batch-mode 2>/dev/null | tail -10 2>/dev/null; then
        info "Performance check completed"
    else
        warning "Could not check performance - pw-top failed"
    fi
else
    info "pw-top not available for performance checking"
fi

# Check input levels
if command -v wpctl >/dev/null 2>&1; then
    echo ""
    echo "Current microphone volume levels:"
    if wpctl get-volume @DEFAULT_AUDIO_SOURCE@ 2>/dev/null; then
        info "Volume check completed"
    else
        warning "Could not get volume info - no default audio source?"
    fi
else
    warning "wpctl not available for volume checking"
fi

echo ""

# 3. Quick fixes
echo "3. Quick Fixes to Try"
echo "===================="
echo ""

echo "Choose a solution to try:"
echo ""
echo "A) Disable automatic RNNoise filter (recommended first step)"
echo "B) Lower microphone input gain"
echo "C) Reduce buffer size for lower latency"
echo "D) Use EasyEffects instead of filter chain"
echo "E) Reset to safe audio settings"
echo "F) Test different sample rates"
echo "G) Monitor audio in real-time"
echo "H) All of the above (comprehensive fix)"
echo ""

read -p "Enter your choice (A-H): " choice

case $choice in
    A|a)
        echo ""
        highlight "Disabling automatic RNNoise filter..."
        if command -v pw-dump >/dev/null 2>&1 && command -v jq >/dev/null 2>&1 && command -v pw-cli >/dev/null 2>&1; then
            # Find and remove RNNoise filter nodes
            FILTER_IDS=$(pw-dump 2>/dev/null | jq -r '.[] | select(.info.props."node.name" == "rnnoise_source") | .id' 2>/dev/null || echo "")
            if [ -n "$FILTER_IDS" ]; then
                echo "$FILTER_IDS" | while read -r id; do
                    if [ -n "$id" ]; then
                        echo "Removing filter node $id"
                        pw-cli destroy "$id" 2>/dev/null || warning "Could not remove filter $id"
                    fi
                done
                success "RNNoise filter removal attempted"
            else
                info "No RNNoise filter found to remove"
            fi
            echo "Try speaking now. If distortion is gone, use EasyEffects for noise suppression instead."
        else
            warning "Required tools not available (pw-dump, jq, pw-cli)"
            echo "Try manually: systemctl --user restart pipewire"
        fi
        ;;
        
    B|b)
        echo ""
        highlight "Lowering microphone input gain to 50%..."
        wpctl set-volume @DEFAULT_AUDIO_SOURCE@ 50%
        success "Microphone gain reduced to 50%"
        echo "Test your voice now. Adjust further if needed with: wpctl set-volume @DEFAULT_AUDIO_SOURCE@ X%"
        ;;
        
    C|c)
        echo ""
        highlight "Setting lower buffer size for reduced latency..."
        pw-metadata -n settings 0 clock.force-quantum 512
        success "Buffer size set to 512 samples"
        echo "This should reduce latency but may increase CPU usage"
        ;;
        
    D|d)
        echo ""
        highlight "Launching EasyEffects for manual noise suppression..."
        if command -v easyeffects >/dev/null 2>&1; then
            easyeffects &
            success "EasyEffects launched"
            echo ""
            echo "In EasyEffects:"
            echo "1. Go to 'Input' tab"
            echo "2. Add 'RNNoise' effect"
            echo "3. Set 'VAD Threshold' to 95% (very conservative)"
            echo "4. Set 'Wet' signal to 50-70% (not 100%)"
            echo "5. Disable any other aggressive processing"
        else
            error "EasyEffects not available"
        fi
        ;;
        
    E|e)
        echo ""
        highlight "Resetting to safe audio settings..."
        # Reset quantum
        pw-metadata -n settings 0 clock.force-quantum 0
        # Reset rate
        pw-metadata -n settings 0 clock.force-rate 0
        # Set reasonable volume
        wpctl set-volume @DEFAULT_AUDIO_SOURCE@ 70%
        # Restart audio services
        systemctl --user restart pipewire pipewire-pulse wireplumber
        success "Audio settings reset to defaults"
        echo "Wait 5 seconds for services to restart, then test your voice"
        ;;
        
    F|f)
        echo ""
        highlight "Testing different sample rates..."
        echo "Current rate: $(pw-metadata -n settings | grep clock.rate | awk '{print $3}' || echo 'default')"
        echo ""
        echo "Trying 44100 Hz..."
        pw-metadata -n settings 0 clock.force-rate 44100
        sleep 2
        echo "Test your voice now. Press Enter to continue..."
        read
        echo "Trying 48000 Hz..."
        pw-metadata -n settings 0 clock.force-rate 48000
        sleep 2
        echo "Test your voice now. Press Enter to continue..."
        read
        echo "Back to automatic rate..."
        pw-metadata -n settings 0 clock.force-rate 0
        success "Rate testing complete"
        ;;
        
    G|g)
        echo ""
        highlight "Starting real-time audio monitoring..."
        echo "Press Ctrl+C to stop monitoring"
        echo ""
        if command -v pw-top >/dev/null 2>&1; then
            pw-top
        else
            echo "Monitoring with wpctl status (updating every 2 seconds):"
            while true; do
                clear
                echo "=== PipeWire Status ==="
                wpctl status
                echo ""
                echo "=== Microphone Volume ==="
                wpctl get-volume @DEFAULT_AUDIO_SOURCE@
                echo ""
                echo "Press Ctrl+C to stop"
                sleep 2
            done
        fi
        ;;
        
    H|h)
        echo ""
        highlight "Running comprehensive fix..."
        
        # Step 1: Disable RNNoise filter
        echo "1/6: Disabling automatic RNNoise filter..."
        if command -v pw-dump >/dev/null 2>&1 && command -v jq >/dev/null 2>&1; then
            FILTER_IDS=$(pw-dump 2>/dev/null | jq -r '.[] | select(.info.props."node.name" == "rnnoise_source") | .id' 2>/dev/null || echo "")
            if [ -n "$FILTER_IDS" ]; then
                echo "$FILTER_IDS" | while read -r id; do
                    if [ -n "$id" ]; then
                        pw-cli destroy "$id" 2>/dev/null || true
                    fi
                done
            fi
        fi
        
        # Step 2: Reset audio settings
        echo "2/6: Resetting audio settings..."
        pw-metadata -n settings 0 clock.force-quantum 0 2>/dev/null || true
        pw-metadata -n settings 0 clock.force-rate 0 2>/dev/null || true
        
        # Step 3: Set conservative volume
        echo "3/6: Setting conservative microphone gain..."
        wpctl set-volume @DEFAULT_AUDIO_SOURCE@ 60% 2>/dev/null || warning "Could not set volume"
        
        # Step 4: Restart services
        echo "4/6: Restarting audio services..."
        systemctl --user restart pipewire pipewire-pulse wireplumber 2>/dev/null || warning "Could not restart services"
        
        # Step 5: Wait for restart
        echo "5/6: Waiting for services to stabilize..."
        sleep 5
        
        # Step 6: Launch EasyEffects
        echo "6/6: Launching EasyEffects for manual control..."
        if command -v easyeffects >/dev/null 2>&1; then
            easyeffects &
            success "Comprehensive fix applied!"
            echo ""
            echo "Next steps:"
            echo "1. Test your voice without any effects first"
            echo "2. In EasyEffects, gradually add noise suppression:"
            echo "   - Start with RNNoise at 50% wet signal"
            echo "   - Use VAD threshold of 95% or higher"
            echo "   - Avoid aggressive compression or EQ"
            echo "3. If still distorted, try lowering input gain further"
        else
            warning "EasyEffects not available for manual control"
        fi
        ;;
        
    *)
        error "Invalid choice"
        ;;
esac

echo ""
echo "🎯 Additional Tips to Prevent Distortion:"
echo "========================================="
echo ""
echo "• Keep microphone gain below 80% to avoid clipping"
echo "• Use RNNoise conservatively (50-70% wet signal, not 100%)"
echo "• Check for background applications using audio"
echo "• Ensure your microphone hardware supports 48kHz"
echo "• Consider using a better quality microphone"
echo "• Avoid stacking multiple noise reduction effects"
echo ""

echo "Run this script again anytime with: troubleshoot-voice-distortion"
echo ""
echo "✅ Script completed successfully!"
exit 0
