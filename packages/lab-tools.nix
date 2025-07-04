{
  lib,
  stdenv,
  guile,
  makeWrapper,
}: let
  # Lab - K.I.S.S Refactored Implementation
  lab = stdenv.mkDerivation {
    pname = "lab";
    version = "0.1.0-dev";

    src = ./lab-tool;

    nativeBuildInputs = [makeWrapper];
    buildInputs = [guile];

    installPhase = ''
            set -e
            echo "Current directory: $(pwd)"
            ls -l
            mkdir -p $out/share/lab
            cp -r core main deploy $out/share/lab/
            cp main.scm $out/share/lab/
            echo "After copy:"
            find $out/share/lab
            mkdir -p $out/bin
            cat > $out/bin/lab << EOF
      #!/usr/bin/env bash
      export GUILE_LOAD_PATH="$out/share/lab:\$GUILE_LOAD_PATH"
      exec ${guile}/bin/guile "$out/share/lab/main.scm" "\$@"
      EOF
            chmod +x $out/bin/lab
    '';

    meta = with lib; {
      description = "Minimal, functional home lab management tool in Guile Scheme";
      longDescription = ''
        A modular, functional home lab management tool following K.I.S.S principles:
        - Infrastructure status checking
        - Machine management and deployment
        - SSH connectivity testing and extension
        - Pure functions separated from side effects
        - Simple, single-responsibility modules
      '';
      homepage = "https://github.com/geirda/Home-lab";
      license = licenses.mit;
      maintainers = ["geir"];
      platforms = platforms.unix;
    };
  };
in {
  # Export only the lab tool
  lab = lab;
  default = lab;
}
