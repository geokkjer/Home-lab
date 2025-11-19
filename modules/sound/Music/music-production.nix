{
  # Music Production Module - Wrapper for backward compatibility
  # All music production software is now consolidated in ../../sound/music-software.nix
  # This file is kept for backward compatibility with existing configurations

  imports = [
    ../../sound/music-software.nix
  ];
}
