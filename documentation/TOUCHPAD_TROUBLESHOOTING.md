# Touchpad Troubleshooting - Little Rascal (Lenovo Yoga Slim 7)

## Problem
The touchpad on little-rascal (Lenovo Yoga Slim 7 14ARE05) is not working. The trackpad is detected as an I2C HID device but is being misidentified as a sensor hub instead of a proper touchpad.

## Hardware Details
- **Model**: Lenovo Yoga Slim 7 14ARE05
- **Touchpad Controller**: ITE8353 (I2C HID)
- **Device Path**: `/sys/bus/i2c/devices/i2c-ITE8353:00`
- **HID Device**: `0018:048D:8353.0001`
- **ACPI ID**: `PNP0C50` (HID-over-I2C precision touchpad)

## Investigation Results

### Device Detection
The touchpad controller is properly detected by the kernel:
- I2C device exists at `i2c-ITE8353:00`
- Driver binding: `i2c_hid_acpi`
- HID device: `0018:048D:8353.0001`

### Problem: Misidentified as Sensor Hub
The device is being bound to `hid-sensor-hub` driver instead of a proper touchpad driver like `hid-multitouch`.

### Attempted Solutions

1. **Added I2C HID kernel modules**:
   - `i2c_hid`, `i2c_hid_acpi`, `hid_multitouch`
   - Added to both `boot.kernelModules` and `boot.initrd.availableKernelModules`

2. **Added kernel parameters**:
   - `i2c_hid.debug=1`
   - `acpi_enforce_resources=lax`
   - `i2c_hid_acpi.probe_defer=1`

3. **Added udev rules**:
   - Attempted to unbind from `hid-sensor-hub` and bind to `hid-multitouch`
   - Set proper permissions for the device

4. **Enabled libinput**:
   - Configured touchpad settings in `modules/desktop/input.nix`
   - Added input utilities (`evtest`, `xinput`, `libinput-gestures`)

### Current Status
- ❌ Touchpad still not functional
- ✅ Device is detected by kernel
- ❌ Device is misidentified as sensor hub
- ❌ No input events generated

## Next Steps to Try

### Option 1: BIOS Settings
Check BIOS for touchpad mode settings:
- Look for "Touchpad Mode" setting
- Try switching between "Basic" and "Advanced" modes
- Some laptops have I2C/PS2 mode selection

### Option 2: Kernel Patch
This appears to be a known issue requiring a kernel patch. Research:
- Linux kernel patches for ITE8353 touchpad support
- Check if newer kernels have better support
- Look for device-specific quirks in the kernel

### Option 3: Force PS/2 Mode
If I2C mode cannot be made to work:
- Try to enable PS/2 emulation mode in BIOS
- Some laptops can fall back to PS/2 touchpad mode

### Option 4: Custom Driver
- Check if ITE provides Linux drivers
- Look for community-developed drivers for this specific controller

### Option 5: Firmware Update
- Update laptop firmware/BIOS
- Some touchpad issues are resolved with firmware updates

## Useful Commands for Further Debugging

```bash
# Check device binding
ls -la /sys/bus/i2c/devices/i2c-ITE8353:00/driver

# Check HID devices
cat /sys/bus/i2c/devices/i2c-ITE8353:00/0018:048D:8353.0001/modalias

# Monitor udev events
udevadm monitor --environment --udev

# Test input events
evtest

# Check kernel modules
lsmod | grep -E "(i2c|hid)"
```

## References
- [NixOS Hardware Configuration](https://github.com/NixOS/nixos-hardware)
- [Linux I2C HID Documentation](https://www.kernel.org/doc/html/latest/input/devices/i2c-hid.html)
- [Libinput Documentation](https://wayland.freedesktop.org/libinput/doc/latest/)
