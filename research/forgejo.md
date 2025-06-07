# Forgejo Best Practices and Configuration Guide

## Overview

Forgejo is a self-hosted lightweight software forge focused on scaling, federation, and privacy. This document outlines best practices for configuring and securing a Forgejo instance, along with potential improvements for the current setup.

## Current Configuration Analysis

### Current Setup Summary
- **Service**: Forgejo running on grey-area machine
- **User**: `git` user (follows best practices)
- **Mode**: Production (`RUN_MODE = "prod"`)
- **URL**: `https://git.geokkjer.eu`
- **SSH**: Port 1337 on `git.geokkjer.eu`
- **Registration**: Disabled (security best practice)

### Configuration Location
- Configuration file: `/home/geir/Home-lab/machines/grey-area/services/forgejo.nix`

## Security Best Practices

### 1. Authentication and Access Control

**Current Status**: ✅ Registration disabled
```nix
service = {
  DISABLE_REGISTRATION = true;
};
```

**Recommendations**:
- Consider implementing OAuth2/OIDC for centralized authentication
- Enable two-factor authentication (2FA)
- Set up proper user management policies

**Potential Improvements**:
```nix
service = {
  DISABLE_REGISTRATION = true;
  REQUIRE_SIGNIN_VIEW = true;  # Require login to view repos
  ENABLE_CAPTCHA = true;       # Enable captcha for forms
  DEFAULT_ALLOW_CREATE_ORGANIZATION = false;
};

security = {
  INSTALL_LOCK = true;
  SECRET_KEY = "your-secret-key-here";  # Use secrets management
  LOGIN_REMEMBER_DAYS = 7;
  COOKIE_REMEMBER_NAME = "forgejo_incredible";
  COOKIE_USERNAME = "forgejo_username";
  COOKIE_SECURE = true;  # HTTPS only cookies
  ENABLE_LOGIN_STATUS_COOKIE = true;
};
```

### 2. SSH Configuration

**Current Status**: ✅ Custom SSH port (1337)
```nix
server = {
  SSH_PORT = 1337;
};
```

**Additional SSH Security**:
```nix
ssh = {
  DISABLE_SSH = false;
  START_SSH_SERVER = true;
  SSH_SERVER_HOST_KEYS = "ssh/forgejo.rsa, ssh/gogs.rsa";
  SSH_KEY_TEST_PATH = "/tmp";
  SSH_KEYGEN_PATH = "ssh-keygen";
  SSH_SERVER_CIPHERS = "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr";
  SSH_SERVER_KEY_EXCHANGES = "curve25519-sha256@libssh.org,ecdh-sha2-nistp256,ecdh-sha2-nistp384,ecdh-sha2-nistp521,diffie-hellman-group14-sha256,diffie-hellman-group14-sha1";
  SSH_SERVER_MACS = "hmac-sha2-256-etm@openssh.com,hmac-sha2-256,hmac-sha1";
};
```

### 3. Database Security

**Recommendation**: Use PostgreSQL instead of SQLite for production
```nix
database = {
  DB_TYPE = "postgres";
  HOST = "127.0.0.1:5432";
  NAME = "forgejo";
  USER = "forgejo";
  PASSWD = "secure-password";  # Use secrets management
  SSL_MODE = "require";
  CHARSET = "utf8";
};
```

## Performance Optimization

### 1. Caching Configuration
```nix
cache = {
  ADAPTER = "redis";
  INTERVAL = 60;
  HOST = "127.0.0.1:6379";
  ITEM_TTL = "16h";
};

session = {
  PROVIDER = "redis";
  PROVIDER_CONFIG = "127.0.0.1:6379";
  COOKIE_NAME = "i_like_forgejo";
  COOKIE_SECURE = true;
  GC_INTERVAL_TIME = 86400;
  SESSION_LIFE_TIME = 86400;
  DOMAIN = "git.geokkjer.eu";
};
```

### 2. Repository Management
```nix
repository = {
  ENABLE_PUSH_CREATE_USER = true;  # Already configured ✅
  ENABLE_PUSH_CREATE_ORG = false;
  DEFAULT_BRANCH = "main";
  PREFERRED_LICENSES = "Apache License 2.0,MIT License,GPL-3.0-or-later";
  DISABLE_HTTP_GIT = false;
  ACCESS_CONTROL_ALLOW_ORIGIN = "";
  USE_COMPAT_SSH_URI = false;
  DEFAULT_CLOSE_ISSUES_VIA_COMMITS_IN_ANY_BRANCH = false;
  ENABLE_PUSH_CREATE_USER = true;
  ENABLE_PUSH_CREATE_ORG = false;
};
```

### 3. Indexer Configuration
```nix
indexer = {
  ISSUE_INDEXER_TYPE = "bleve";
  ISSUE_INDEXER_PATH = "indexers/issues.bleve";
  REPO_INDEXER_ENABLED = true;
  REPO_INDEXER_PATH = "indexers/repos.bleve";
  UPDATE_BUFFER_LEN = 20;
  MAX_FILE_SIZE = 1048576;
};
```

## Backup and Disaster Recovery

### 1. Data Backup Strategy
```bash
# Database backup (if using PostgreSQL)
pg_dump -U forgejo -h localhost forgejo > forgejo_backup_$(date +%Y%m%d).sql

# Repository data backup
tar -czf forgejo_repos_$(date +%Y%m%d).tar.gz /var/lib/forgejo/data/

# Configuration backup
cp /etc/forgejo/app.ini forgejo_config_$(date +%Y%m%d).ini
```

### 2. Automated Backup with NixOS
```nix
# Add to configuration.nix
services.postgresqlBackup = {
  enable = true;
  databases = [ "forgejo" ];
  startAt = "daily";
  location = "/backup/postgresql";
};

systemd.services.forgejo-backup = {
  description = "Backup Forgejo repositories";
  startAt = "daily";
  script = ''
    ${pkgs.gnutar}/bin/tar -czf /backup/forgejo/repos_$(date +%Y%m%d).tar.gz /var/lib/forgejo/data/
  '';
  serviceConfig = {
    Type = "oneshot";
    User = "root";
  };
};
```

## Monitoring and Logging

### 1. Logging Configuration
```nix
log = {
  MODE = "file";
  LEVEL = "Info";
  ROOT_PATH = "/var/log/forgejo";
  REDIRECT_MACARON_LOG = true;
  MACARON_PREFIX = "[Macaron]";
  ROUTER_LOG_LEVEL = "Info";
  ROUTER_PREFIX = "[Router]";
  ENABLE_SSH_LOG = true;
};
```

### 2. Metrics and Monitoring
```nix
metrics = {
  ENABLED = true;
  TOKEN = "your-metrics-token";  # Use secrets management
};

# Add Prometheus monitoring
services.prometheus.exporters.forgejo = {
  enable = true;
  port = 3001;
  configFile = "/etc/forgejo/app.ini";
};
```

## Email Configuration

### SMTP Setup
```nix
mailer = {
  ENABLED = true;
  SMTP_ADDR = "smtp.your-provider.com";
  SMTP_PORT = 587;
  FROM = "noreply@geokkjer.eu";
  USER = "your-smtp-user";
  PASSWD = "your-smtp-password";  # Use secrets management
  ENABLE_HELO = true;
  HELO_HOSTNAME = "git.geokkjer.eu";
  SKIP_VERIFY = false;
  USE_CERTIFICATE = true;
  CERT_FILE = "/path/to/cert.pem";
  KEY_FILE = "/path/to/key.pem";
  IS_TLS_ENABLED = true;
};
```

## Web UI and UX Improvements

### 1. UI Configuration
```nix
ui = {
  EXPLORE_PAGING_NUM = 20;
  ISSUE_PAGING_NUM = 20;
  MEMBERS_PAGING_NUM = 20;
  FEED_MAX_COMMIT_NUM = 5;
  FEED_PAGING_NUM = 20;
  SITEMAP_PAGING_NUM = 20;
  GRAPH_MAX_COMMIT_NUM = 100;
  CODE_COMMENT_LINES = 4;
  DEFAULT_SHOW_FULL_NAME = false;
  SEARCH_REPO_DESCRIPTION = true;
  USE_SERVICE_WORKER = true;
};

"ui.meta" = {
  AUTHOR = "Forgejo";
  DESCRIPTION = "Git with a cup of tea! Painless self-hosted git service.";
  KEYWORDS = "go,git,self-hosted,gitea,forgejo";
};
```

### 2. Webhook Configuration
```nix
webhook = {
  QUEUE_LENGTH = 1000;
  DELIVER_TIMEOUT = 5;
  SKIP_TLS_VERIFY = false;
  ALLOWED_HOST_LIST = "";
  PAGING_NUM = 10;
  PROXY_URL = "";
  PROXY_HOSTS = "";
};
```

## Federation (Future Feature)

Forgejo is working on ActivityPub federation support:

```nix
federation = {
  ENABLED = false;  # Not yet available
  SHARE_USER_STATISTICS = false;
  MAX_SIZE = 4;
  ALGORITHMS = "rsa-sha256,rsa-sha512,ed25519";
};
```

## Potential Improvements for Current Setup

### 1. Immediate Improvements

1. **Add Database Configuration**:
   - Migrate from SQLite to PostgreSQL for better performance
   - Configure connection pooling

2. **Enhance Security**:
   - Add `REQUIRE_SIGNIN_VIEW = true` to require authentication for viewing
   - Configure proper SSL/TLS settings
   - Implement secrets management for sensitive values

3. **Add Monitoring**:
   - Enable metrics collection
   - Set up log rotation
   - Configure health checks

4. **Backup Strategy**:
   - Implement automated backups
   - Set up off-site backup storage

### 2. Medium-term Improvements

1. **Performance Optimization**:
   - Add Redis for caching and sessions
   - Configure repository indexing
   - Optimize garbage collection

2. **User Experience**:
   - Configure email notifications
   - Set up custom themes/branding
   - Add webhook integrations

3. **Integration**:
   - Set up CI/CD integration
   - Configure external authentication (LDAP/OAuth)
   - Add container registry support

### 3. Enhanced Configuration Example

```nix
{ pkgs, config, ... }:
{
  services.forgejo = {
    enable = true;
    user = "git";
    stateDir = "/var/lib/forgejo";
    database = {
      type = "postgres";
      host = "127.0.0.1";
      port = 5432;
      name = "forgejo";
      user = "forgejo";
      passwordFile = "/run/secrets/forgejo-db-password";
    };
  };

  services.forgejo.settings = {
    DEFAULT = {
      RUN_MODE = "prod";
      WORK_PATH = "/var/lib/forgejo";
    };
    
    service = {
      DISABLE_REGISTRATION = true;
      REQUIRE_SIGNIN_VIEW = true;
      ENABLE_CAPTCHA = true;
      DEFAULT_ALLOW_CREATE_ORGANIZATION = false;
    };
    
    server = {
      ROOT_URL = "https://git.geokkjer.eu";
      SSH_DOMAIN = "git.geokkjer.eu";
      SSH_PORT = 1337;
      CERT_FILE = "/etc/ssl/certs/forgejo.crt";
      KEY_FILE = "/etc/ssl/private/forgejo.key";
      DISABLE_SSH = false;
      START_SSH_SERVER = true;
    };
    
    repository = {
      ENABLE_PUSH_CREATE_USER = true;
      ENABLE_PUSH_CREATE_ORG = false;
      DEFAULT_BRANCH = "main";
      PREFERRED_LICENSES = "Apache License 2.0,MIT License,GPL-3.0-or-later";
    };
    
    security = {
      INSTALL_LOCK = true;
      SECRET_KEY_PATH = "/run/secrets/forgejo-secret-key";
      LOGIN_REMEMBER_DAYS = 7;
      COOKIE_SECURE = true;
      ENABLE_LOGIN_STATUS_COOKIE = true;
    };
    
    log = {
      MODE = "file";
      LEVEL = "Info";
      ROOT_PATH = "/var/log/forgejo";
      REDIRECT_MACARON_LOG = true;
    };
    
    metrics = {
      ENABLED = true;
      TOKEN_PATH = "/run/secrets/forgejo-metrics-token";
    };
    
    mailer = {
      ENABLED = true;
      SMTP_ADDR = "smtp.fastmail.com";
      SMTP_PORT = 587;
      FROM = "noreply@geokkjer.eu";
      USER = "forgejo@geokkjer.eu";
      PASSWD_PATH = "/run/secrets/forgejo-smtp-password";
      IS_TLS_ENABLED = true;
    };
    
    other = {
      SHOW_FOOTER_VERSION = true;
      SHOW_FOOTER_TEMPLATE_LOAD_TIME = false;
    };
  };

  # PostgreSQL configuration
  services.postgresql = {
    enable = true;
    ensureDatabases = [ "forgejo" ];
    ensureUsers = [
      {
        name = "forgejo";
        ensurePermissions = {
          "DATABASE forgejo" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  # Redis for caching
  services.redis.servers.forgejo = {
    enable = true;
    port = 6379;
    bind = "127.0.0.1";
  };

  # Backup configuration
  services.postgresqlBackup = {
    enable = true;
    databases = [ "forgejo" ];
    startAt = "daily";
    location = "/backup/postgresql";
  };

  # Secrets management
  age.secrets = {
    forgejo-db-password.file = ../secrets/forgejo-db-password.age;
    forgejo-secret-key.file = ../secrets/forgejo-secret-key.age;
    forgejo-metrics-token.file = ../secrets/forgejo-metrics-token.age;
    forgejo-smtp-password.file = ../secrets/forgejo-smtp-password.age;
  };
}
```

## Security Checklist

- [ ] **Authentication**: Disable registration, enable 2FA
- [ ] **Authorization**: Implement proper access controls
- [ ] **Encryption**: Use HTTPS, secure SSH configuration
- [ ] **Database**: Use PostgreSQL with SSL, regular backups
- [ ] **Secrets**: Use proper secrets management (agenix/sops)
- [ ] **Monitoring**: Enable logging, metrics collection
- [ ] **Updates**: Regular security updates, vulnerability scanning
- [ ] **Network**: Firewall configuration, rate limiting
- [ ] **Backup**: Automated backups, disaster recovery plan

## References

- [Forgejo Official Documentation](https://forgejo.org/docs/)
- [Forgejo Configuration Reference](https://forgejo.org/docs/latest/admin/config-cheat-sheet/)
- [Forgejo Security Guide](https://forgejo.org/docs/latest/admin/security/)
- [NixOS Forgejo Module](https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=forgejo)

## Last Updated
June 7, 2025
