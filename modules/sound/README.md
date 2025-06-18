# PipeWire with WirePlumber Configuration

This module provides a comprehensive PipeWire setup with WirePlumber session management, noise suppression, and GUI tools for audio management.

## Features

### Core Audio Stack

- **PipeWire**: Modern audio server with low latency
- **WirePlumber**: Session manager for device management and routing
- **ALSA/PulseAudio/JACK Compatibility**: Works with all major audio APIs
- **Real-time Processing**: RTKit integration for optimal performance

### Noise Suppression

- **RNNoise Plugin**: AI-powered noise suppression for microphones
- **EasyEffects Integration**: GUI for managing audio effects
- **Automatic Filter Chain**: Pre-configured noise suppression pipeline

### GUI Applications Included

- **EasyEffects**: Modern audio effects processor with noise suppression
- **PulseAudio Volume Control (pavucontrol)**: Volume and device management
- **Helvum**: Graphical PipeWire patchbay for routing
- **qpwgraph**: Qt-based PipeWire graph manager
- **pwvucontrol**: Native PipeWire volume control

## Quick Start

### 1. Import the Module

Add to your NixOS configuration:

```nix
imports = [
  ./modules/sound/pipewire.nix
];
```

### 2. Rebuild System

```bash
sudo nixos-rebuild switch
```

### 3. Verify Installation

```bash
# Check if PipeWire is running
systemctl --user status pipewire

# Launch the audio setup helper
audio-setup

# Test microphone with noise suppression
microphone-test
```

## Using Noise Suppression

### Method 1: EasyEffects (Recommended)

1. Launch EasyEffects: `easyeffects` or use the application menu
2. Go to the "Input" tab
3. Load the pre-configured "Microphone_Noise_Suppression" preset
4. Enable the RNNoise effect
5. Adjust the "VAD Threshold" (Voice Activity Detection) as needed

### Method 2: PipeWire Filter Chain (Automatic)

The configuration includes an automatic RNNoise filter chain that creates a "Noise Canceling Source" device. This appears as a separate microphone input in audio applications.

## GUI Applications Usage

### EasyEffects

- **Purpose**: Real-time audio effects and noise suppression
- **Launch**: `easyeffects` or from application menu
- **Features**: RNNoise, equalizer, compressor, limiter, gate
- **Auto-start**: Configured to start with desktop session

### Volume Controls

- **pavucontrol**: Traditional PulseAudio-style interface
- **pwvucontrol**: Native PipeWire interface
- **Usage**: Control volumes, switch devices, manage streams

### Audio Routing

- **Helvum**: Visual patchbay for connecting audio streams
- **qpwgraph**: Advanced graph-based routing interface
- **Usage**: Route audio between applications and devices

## Command-Line Tools

### System Status

```bash
# PipeWire status overview
wpctl status

# Real-time monitoring
pw-top

# Inspect audio objects
pw-dump | jq '.'

# Show metadata
pw-metadata
```

### Device Management

```bash
# List devices
wpctl status

# Set default sink
wpctl set-default SINK_ID

# Set volume
wpctl set-volume SOURCE_ID 80%

# Mute/unmute
wpctl set-mute SOURCE_ID toggle
```

### Testing

```bash
# Test microphone
microphone-test

# Record and playback test
arecord -d 5 -f cd test.wav && aplay test.wav
```

## Configuration Details

### Audio Quality Settings

- **Sample Rate**: 48kHz (professional audio standard)
- **Buffer Size**: 1024 samples (balanced latency/stability)
- **Resampling Quality**: High (level 4)
- **Channels**: Stereo support with spatial audio capabilities

### Noise Suppression Settings

- **RNNoise VAD Threshold**: 50% (adjustable)
- **VAD Grace Period**: 200ms
- **Noise Reduction**: 80% wet signal
- **Processing**: Real-time with minimal latency

### Performance Optimizations

- **Real-time Scheduling**: RTKit enabled
- **Memory Locking**: Enabled for critical processes
- **CPU Affinity**: Configurable per audio thread
- **Quantum Settings**: Optimized for low latency

## Troubleshooting

### Common Issues

#### No Audio Output

```bash
# Check PipeWire is running
systemctl --user restart pipewire pipewire-pulse wireplumber

# Check default devices
wpctl status
```

#### Microphone Not Working

```bash
# Test microphone detection
arecord -l

# Check permissions
groups $USER | grep audio
```

#### High CPU Usage

```bash
# Monitor PipeWire performance
pw-top

# Check buffer settings
pw-metadata | grep quantum
```

#### Noise Suppression Not Working

1. Verify RNNoise plugin is loaded: `ladspa-ls | grep -i noise`
2. Check EasyEffects preset is loaded
3. Ensure correct input device is selected
4. Adjust VAD threshold in EasyEffects

### Reset Configuration

```bash
# Reset user PipeWire configuration
rm -rf ~/.config/pipewire ~/.config/easyeffects
systemctl --user restart pipewire pipewire-pulse wireplumber
```

## Advanced Configuration

### Custom Filter Chains

Edit `/etc/pipewire/pipewire.conf.d/10-noise-suppression.conf` to modify the RNNoise filter chain parameters.

### Device-Specific Settings

Add rules to `/etc/wireplumber/wireplumber.conf.d/51-noise-suppression.conf` for specific audio devices.

### EasyEffects Presets

Custom presets are stored in `/etc/easyeffects/` and can be modified or extended.

## Integration with Applications

### Discord/Zoom/Teams

1. Set default microphone to "Noise Canceling Source" in application settings
2. Or use EasyEffects on the regular microphone input
3. Adjust noise gate and compressor settings as needed

### OBS Studio

1. Add "Application Audio Capture" source
2. Select the noise-suppressed microphone device
3. Or use OBS's built-in noise suppression with the processed audio

### Music Production (JACK)

```bash
# Start JACK mode if needed
pw-jack your-daw-application
```

## Updates and Maintenance

### Updating Configuration

After modifying the Nix configuration:

```bash
sudo nixos-rebuild switch
systemctl --user restart pipewire pipewire-pulse wireplumber
```

### Monitoring Performance

Regular checks recommended:

```bash
# Weekly performance check
pw-top

# Monthly configuration review
audio-setup
```

## See Also

- [PipeWire Documentation](https://docs.pipewire.org/)
- [WirePlumber Documentation](https://pipewire.pages.freedesktop.org/wireplumber/)
- [EasyEffects Documentation](https://github.com/wwmm/easyeffects)
- [RNNoise Project](https://jmvalin.ca/demo/rnnoise/)
