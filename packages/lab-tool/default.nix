{
  lib,
  stdenv,
  makeWrapper,
  guile_3_0,
  guile-ssh,
  guile-json,
  guile-git,
  guile-gcrypt,
  openssh,
  git,
  nixos-rebuild,
}:
stdenv.mkDerivation {
  pname = "lab-tool";
  version = "0.2.0";

  src = ./.;

  buildInputs = [
    guile_3_0
    guile-ssh
    guile-json
    guile-git
    guile-gcrypt
  ];
  nativeBuildInputs = [makeWrapper];

  buildPhase = ''
    # Compile Guile modules for better performance
    mkdir -p $out/share/guile/site/3.0
    cp -r . $out/share/guile/site/3.0/lab-tool/

    # Compile .scm files to .go files
    for file in $(find . -name "*.scm"); do
      echo "Compiling $file"
      guild compile -L . -o $out/share/guile/site/3.0/''${file%.scm}.go $file || true
    done
  '';

  installPhase = ''
        mkdir -p $out/bin

        # Create the main lab executable
        cat > $out/bin/lab << EOF
    #!/usr/bin/env bash
    export GUILE_LOAD_PATH="$out/share/guile/site/3.0/lab-tool:${guile-ssh}/share/guile/site/3.0:${guile-json}/share/guile/site/3.0:${guile-git}/share/guile/site/3.0:${guile-gcrypt}/share/guile/site/3.0:\$GUILE_LOAD_PATH"
    export GUILE_LOAD_COMPILED_PATH="$out/share/guile/site/3.0/lab-tool:${guile-ssh}/lib/guile/3.0/site-ccache:${guile-json}/lib/guile/3.0/site-ccache:${guile-git}/lib/guile/3.0/site-ccache:${guile-gcrypt}/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH"
    exec ${guile_3_0}/bin/guile "$out/share/guile/site/3.0/lab-tool/main.scm" "\$@"
    EOF
        chmod +x $out/bin/lab

        # Create MCP server executable
        cat > $out/bin/lab-mcp-server << EOF
    #!/usr/bin/env bash
    export GUILE_LOAD_PATH="$out/share/guile/site/3.0/lab-tool:${guile-ssh}/share/guile/site/3.0:${guile-json}/share/guile/site/3.0:${guile-git}/share/guile/site/3.0:${guile-gcrypt}/share/guile/site/3.0:\$GUILE_LOAD_PATH"
    export GUILE_LOAD_COMPILED_PATH="$out/share/guile/site/3.0/lab-tool:${guile-ssh}/lib/guile/3.0/site-ccache:${guile-json}/lib/guile/3.0/site-ccache:${guile-git}/lib/guile/3.0/site-ccache:${guile-gcrypt}/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH"
    exec ${guile_3_0}/bin/guile -L "$out/share/guile/site/3.0/lab-tool" -c "(use-modules (mcp server)) (run-mcp-server)"
    EOF
        chmod +x $out/bin/lab-mcp-server

        # Wrap executables with proper environment and Guile library paths
        wrapProgram $out/bin/lab \
          --prefix PATH : ${lib.makeBinPath [openssh git nixos-rebuild]} \
          --prefix GUILE_LOAD_PATH : ${guile-ssh}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_PATH : ${guile-json}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_PATH : ${guile-git}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_PATH : ${guile-gcrypt}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-ssh}/lib/guile/3.0/site-ccache \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-json}/lib/guile/3.0/site-ccache \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-git}/lib/guile/3.0/site-ccache \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-gcrypt}/lib/guile/3.0/site-ccache

        wrapProgram $out/bin/lab-mcp-server \
          --prefix PATH : ${lib.makeBinPath [openssh git nixos-rebuild]} \
          --prefix GUILE_LOAD_PATH : ${guile-ssh}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_PATH : ${guile-json}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_PATH : ${guile-git}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_PATH : ${guile-gcrypt}/share/guile/site/3.0 \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-ssh}/lib/guile/3.0/site-ccache \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-json}/lib/guile/3.0/site-ccache \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-git}/lib/guile/3.0/site-ccache \
          --prefix GUILE_LOAD_COMPILED_PATH : ${guile-gcrypt}/lib/guile/3.0/site-ccache
  '';

  meta = with lib; {
    description = "Home Lab Tool - Guile implementation with MCP integration";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = ["geir@home-lab"];
  };
}
