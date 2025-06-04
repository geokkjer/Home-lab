# Packages Directory

This directory contains custom package definitions and overlays for the Home-lab NixOS infrastructure.

## Directory Purpose

The `packages/` directory is used for:
- Custom package derivations not available in nixpkgs
- Modified versions of existing packages
- Home-lab specific applications and utilities
- Package overlays and customizations

## Structure

### Custom Packages
- `my-package/` - Individual package directories
- `default.nix` - Package collection and exports
- `flake-module.nix` - Flake integration for packages

### Package Categories
- `applications/` - Custom applications and GUIs
- `scripts/` - Shell scripts and automation tools
- `configs/` - Configuration packages and templates
- `overlays/` - Package overlays and modifications

## Usage

### In Flake Configuration
```nix
# flake.nix
{
  outputs = { self, nixpkgs, ... }: {
    packages.x86_64-linux = import ./packages { 
      pkgs = nixpkgs.legacyPackages.x86_64-linux; 
    };
    
    overlays.default = import ./packages/overlays;
  };
}
```

### In Machine Configuration
```nix
# machine configuration
{
  nixpkgs.overlays = [ inputs.self.overlays.default ];
  
  environment.systemPackages = with pkgs; [
    # Custom packages from this directory
    my-custom-tool
    home-lab-scripts
  ];
}
```

## Package Development

### Creating New Package
1. Create package directory: `packages/my-package/`
2. Write `default.nix` with package derivation
3. Add to `packages/default.nix` exports
4. Test with `nix build .#my-package`

### Package Template
```nix
{ lib, stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation rec {
  pname = "my-package";
  version = "1.0.0";
  
  src = fetchFromGitHub {
    owner = "user";
    repo = "repo";
    rev = "v${version}";
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };
  
  meta = with lib; {
    description = "Description of my package";
    homepage = "https://github.com/user/repo";
    license = licenses.mit;
    maintainers = [ "geir" ];
    platforms = platforms.linux;
  };
}
```

## Overlay Examples

### Package Modification
```nix
# overlays/default.nix
final: prev: {
  # Modify existing package
  vim = prev.vim.override {
    features = "huge";
  };
  
  # Add custom package
  home-lab-tools = final.callPackage ../tools { };
}
```

## Home-lab Specific Packages

### CongenitalOptimist Packages
- Development environment customizations
- Workstation-specific tools
- Desktop application modifications

### sleeper-service Packages
- File server utilities
- Monitoring tools
- Backup scripts
- Network service tools

## Best Practices

- **Versioning**: Pin package versions for reproducibility
- **Documentation**: Include clear descriptions and usage
- **Testing**: Test packages across target machines
- **Licensing**: Respect upstream licenses and attributions
- **Maintenance**: Keep packages updated and functional

## Integration with Modules

Packages can be integrated with NixOS modules:
```nix
# modules/development/tools.nix
{ config, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    # Reference custom packages
    home-lab-dev-tools
    custom-editor-config
  ];
}
```

## Flake Outputs

Custom packages are exported as flake outputs:
- `packages.x86_64-linux.package-name`
- `overlays.default`
- `apps.x86_64-linux.script-name`