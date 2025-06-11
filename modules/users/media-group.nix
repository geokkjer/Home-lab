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

  # Create media user for NFS anonymous mapping
  users.users.media = {
    uid = 993; # Fixed UID matching GID for NFS squashing
    group = "media";
    isSystemUser = true;
    description = "Media files user for NFS squashing";
    shell = "/run/current-system/sw/bin/nologin";
  };
}
