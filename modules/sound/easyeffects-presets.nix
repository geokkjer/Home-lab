{
  config,
  lib,
  pkgs,
  ...
}: {
  # Create EasyEffects configuration directory and presets
  environment.etc = {
    # Input preset for microphone noise suppression
    "easyeffects/input/Microphone_Noise_Suppression.json".text = builtins.toJSON {
      input = {
        blocklist = [];
        equalizer = {
          balance = 0.0;
          bypass = false;
          input-gain = 0.0;
          left = {
            band0 = {
              frequency = 29.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band1 = {
              frequency = 59.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band2 = {
              frequency = 119.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band3 = {
              frequency = 237.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band4 = {
              frequency = 474.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band5 = {
              frequency = 947.0;
              gain = 2.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band6 = {
              frequency = 1889.0;
              gain = 1.5;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band7 = {
              frequency = 3770.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band8 = {
              frequency = 7523.0;
              gain = 0.5;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band9 = {
              frequency = 15011.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
          };
          mode = "IIR";
          num-bands = 10;
          output-gain = 0.0;
          pitch-left = 0.0;
          pitch-right = 0.0;
          right = {
            band0 = {
              frequency = 29.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band1 = {
              frequency = 59.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band2 = {
              frequency = 119.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band3 = {
              frequency = 237.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band4 = {
              frequency = 474.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band5 = {
              frequency = 947.0;
              gain = 2.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band6 = {
              frequency = 1889.0;
              gain = 1.5;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band7 = {
              frequency = 3770.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band8 = {
              frequency = 7523.0;
              gain = 0.5;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band9 = {
              frequency = 15011.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
          };
          split-channels = false;
        };
        filter = {
          balance = 0.0;
          bypass = false;
          frequency = 2000.0;
          gain = 0.0;
          mode = "12dB/oct Highpass";
          quality = 0.7071;
          slope = "x1";
        };
        gate = {
          attack = 20.0;
          bypass = false;
          curve-threshold = -24.0;
          curve-zone = 2.0;
          hpf-frequency = 10.0;
          hpf-mode = "12dB/oct Highpass";
          input-gain = 0.0;
          knee = 2.5;
          lpf-frequency = 20000.0;
          lpf-mode = "12dB/oct Lowpass";
          makeup = 0.0;
          ratio = 2.0;
          release = 250.0;
          sidechain = {
            lookahead = 0.0;
            mode = "RMS";
            preamp = 0.0;
            reactivity = 10.0;
            source = "Middle";
          };
          threshold = -18.0;
        };
        limiter = {
          alr = false;
          alr-attack = 5.0;
          alr-knee = 0.0;
          alr-release = 50.0;
          attack = 5.0;
          bypass = false;
          dithering = "None";
          external-sidechain = false;
          gain-boost = true;
          input-gain = 0.0;
          lookahead = 5.0;
          mode = "Herm Thin";
          output-gain = 0.0;
          oversampling = "None";
          release = 5.0;
          sidechain-preamp = 0.0;
          stereo-link = 100.0;
          threshold = 0.0;
        };
        plugins_order = [
          "filter"
          "gate"
          "equalizer"
          "rnnoise"
          "limiter"
        ];
        rnnoise = {
          bypass = false;
          enable-vad = true;
          input-gain = 0.0;
          model-path = "";
          output-gain = 0.0;
          release = 20.0;
          vad-thres = 50.0;
          wet = 80.0;
        };
      };
    };

    # Output preset for speakers/headphones
    "easyeffects/output/Speakers_Enhanced.json".text = builtins.toJSON {
      output = {
        blocklist = [];
        equalizer = {
          balance = 0.0;
          bypass = false;
          input-gain = 0.0;
          left = {
            band0 = {
              frequency = 29.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band1 = {
              frequency = 59.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band2 = {
              frequency = 119.0;
              gain = 0.5;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band3 = {
              frequency = 237.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band4 = {
              frequency = 474.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band5 = {
              frequency = 947.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band6 = {
              frequency = 1889.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band7 = {
              frequency = 3770.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band8 = {
              frequency = 7523.0;
              gain = 2.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band9 = {
              frequency = 15011.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
          };
          mode = "IIR";
          num-bands = 10;
          output-gain = 0.0;
          pitch-left = 0.0;
          pitch-right = 0.0;
          right = {
            band0 = {
              frequency = 29.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band1 = {
              frequency = 59.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band2 = {
              frequency = 119.0;
              gain = 0.5;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band3 = {
              frequency = 237.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band4 = {
              frequency = 474.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band5 = {
              frequency = 947.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band6 = {
              frequency = 1889.0;
              gain = 0.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band7 = {
              frequency = 3770.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band8 = {
              frequency = 7523.0;
              gain = 2.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
            band9 = {
              frequency = 15011.0;
              gain = 1.0;
              mode = "RLC (BT)";
              mute = false;
              q = 4.36;
              slope = "x1";
              solo = false;
              type = "Bell";
            };
          };
          split-channels = false;
        };
        limiter = {
          alr = false;
          alr-attack = 5.0;
          alr-knee = 0.0;
          alr-release = 50.0;
          attack = 5.0;
          bypass = false;
          dithering = "None";
          external-sidechain = false;
          gain-boost = true;
          input-gain = 0.0;
          lookahead = 5.0;
          mode = "Herm Thin";
          output-gain = 0.0;
          oversampling = "None";
          release = 5.0;
          sidechain-preamp = 0.0;
          stereo-link = 100.0;
          threshold = -1.0;
        };
        plugins_order = [
          "equalizer"
          "limiter"
        ];
      };
    };
  };
}
