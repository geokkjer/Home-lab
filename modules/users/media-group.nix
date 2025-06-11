# Media Group Configuration
# Shared group for NFS media access permissions
{
  config,
  pkgs,
  ...
}: {
  # Create the media group for shared NFS access
  users.groups.media = {
    gid = 993; # Fixed GID for consistency across machines
  };
}
