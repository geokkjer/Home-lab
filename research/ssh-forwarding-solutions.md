# SSH Forwarding Solutions for Git Server

## Overview

This document researches solutions for forwarding SSH traffic from the reverse-proxy (VPS) to the grey-area Forgejo git instance. The goal is to enable Git operations over SSH at `git@git.geokkjer.eu` while maintaining security and reliability.

## Current Setup

- **Reverse Proxy (VPS)**: `46.226.104.98` running nginx, listening on ports 80/443
- **Git Server (grey-area)**: Forgejo instance accessible via Tailscale at `apps:3000`
- **Domain**: `git.geokkjer.eu` currently configured for HTTPS proxying only
- **Target**: Enable SSH access on port 22 for Git operations

## Architecture Constraints

1. **Single Public IP**: Only one public IP available (VPS)
2. **Port 22 Limitation**: Standard SSH port needed for Git compatibility
3. **Security**: SSH access should be restricted to Git operations only
4. **Tailscale Network**: Backend services accessible via private Tailscale network
5. **Existing SSH**: VPS already uses port 22 for administrative SSH access

## Solution Options

### 1. HAProxy TCP Mode (Recommended)

**Approach**: Replace nginx with HAProxy or add HAProxy for SSH forwarding

**Pros**:
- Native TCP/SSH forwarding support
- Battle-tested for SSH proxying
- Can handle both HTTP and SSH traffic
- Excellent connection handling
- Built-in health checking

**Cons**:
- Requires replacing or complementing nginx
- Additional service to manage
- More complex configuration

**Implementation**:
```haproxy
global
    stats socket /var/run/haproxy.sock mode 600 level admin
    stats timeout 2m

defaults
    mode tcp
    timeout client 60s
    timeout server 60s
    timeout connect 5s

# SSH forwarding to Git server
frontend ssh_frontend
    bind *:22
    mode tcp
    # Use SNI or other method to distinguish Git SSH from admin SSH
    default_backend git_ssh_backend

backend git_ssh_backend
    mode tcp
    server git1 apps:22 check

# HTTP/HTTPS forwarding
frontend http_frontend
    bind *:80
    bind *:443 ssl crt /etc/ssl/certs/
    mode http
    default_backend nginx_backend

backend nginx_backend
    mode http
    server nginx1 127.0.0.1:8080
```

**Complexity**: Medium
**Security**: High
**Reliability**: High

### 2. NGINX Stream Module

**Approach**: Enable NGINX stream module for TCP forwarding

**Pros**:
- Keeps existing nginx setup
- Native TCP proxying support
- Good performance
- Integrated with current SSL/certificate management

**Cons**:
- Requires nginx with stream module compiled
- Cannot distinguish SSH traffic types on same port
- Conflicts with existing SSH access

**Implementation**:
```nginx
# In nginx.conf
stream {
    upstream git_ssh_backend {
        server apps:22;
    }
    
    server {
        listen 2222;  # Alternative port for Git SSH
        proxy_pass git_ssh_backend;
        proxy_timeout 60s;
        proxy_connect_timeout 5s;
    }
}

http {
    # Existing HTTP configuration
    server {
        listen 80;
        listen 443 ssl;
        server_name git.geokkjer.eu;
        # ... existing config
    }
}
```

**Complexity**: Low
**Security**: Medium (requires alternative port)
**Reliability**: High

### 3. SSH Port Forwarding with systemd

**Approach**: Use SSH tunneling with autossh/systemd for persistence

**Pros**:
- Simple setup
- Uses existing SSH infrastructure
- Automatic reconnection
- No additional services

**Cons**:
- Less robust than dedicated proxy
- Potential authentication complexity
- Single point of failure
- Not designed for high-throughput Git operations

**Implementation**:
```bash
# SSH tunnel command
ssh -N -L 2222:apps:22 geir@grey-area-tailscale-ip

# systemd service for persistence
[Unit]
Description=SSH tunnel for Git forwarding
After=network.target

[Service]
Type=simple
User=git
ExecStart=/usr/bin/ssh -N -o ServerAliveInterval=30 -o ServerAliveCountMax=3 -L 2222:apps:22 geir@grey-area
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
```

**Complexity**: Low
**Security**: Medium
**Reliability**: Medium

### 4. iptables DNAT (Network Address Translation)

**Approach**: Use iptables to redirect SSH traffic to backend

**Pros**:
- Kernel-level forwarding (fast)
- Transparent to applications
- No additional services
- Can work on same port

**Cons**:
- Requires careful firewall management
- Complex routing setup with Tailscale
- Difficult to debug
- Less visibility into connections

**Implementation**:
```bash
# Forward port 2222 to backend Git server
iptables -t nat -A PREROUTING -p tcp --dport 2222 -j DNAT --to-destination apps:22
iptables -t nat -A POSTROUTING -p tcp -d apps --dport 22 -j MASQUERADE

# Allow forwarding
iptables -A FORWARD -p tcp --dport 22 -d apps -j ACCEPT
```

**Complexity**: High
**Security**: Medium
**Reliability**: Medium

### 5. Hybrid Solution: Port-Based Routing

**Approach**: Use different ports for different SSH services

**Pros**:
- Clear separation of concerns
- Maintains existing admin SSH on port 22
- Simple to implement and debug
- Good security boundaries

**Cons**:
- Non-standard Git SSH port
- Requires client configuration
- Less user-friendly

**Implementation**:
```nginx
stream {
    upstream git_ssh {
        server apps:22;
    }
    
    server {
        listen 2222;  # Git SSH port
        proxy_pass git_ssh;
        proxy_timeout 300s;
        proxy_connect_timeout 10s;
    }
}
```

**Client Configuration**:
```bash
# ~/.ssh/config
Host git.geokkjer.eu
    Port 2222
    User git
```

**Complexity**: Low
**Security**: High
**Reliability**: High

## Recommended Solution

### Phase 1: Implement NGINX Stream Module with Alternative Port

**Why**: 
- Lowest risk implementation
- Maintains existing services
- Easy to test and validate
- Can be upgraded later

**Configuration**:
1. Enable nginx stream module
2. Configure Git SSH on port 2222
3. Update Forgejo SSH_DOMAIN setting
4. Test with alternative port

### Phase 2: Consider HAProxy Migration (Future)

**Why**:
- More robust for mixed TCP/HTTP traffic
- Better connection handling for Git operations
- Industry standard for this use case
- Enables port 22 for Git SSH

## Security Considerations

1. **SSH Key Management**: Ensure proper SSH key authentication
2. **Rate Limiting**: Implement connection rate limits
3. **Monitoring**: Log SSH connections for security auditing  
4. **Firewall Rules**: Restrict SSH forwarding to specific sources if possible
5. **Fail2ban**: Extend fail2ban rules to cover Git SSH attempts

## Testing Strategy

1. **Basic Connectivity**: Test TCP connection to backend
2. **SSH Handshake**: Verify SSH protocol negotiation
3. **Git Operations**: Test clone, push, pull operations
4. **Performance**: Measure latency and throughput
5. **Failover**: Test behavior during backend unavailability

## Implementation Priority

1. **High Priority**: NGINX Stream Module (Phase 1)
2. **Medium Priority**: HAProxy migration evaluation
3. **Low Priority**: Advanced features (rate limiting, monitoring)

## Next Steps

1. Check if nginx has stream module compiled
2. Implement NGINX stream configuration for port 2222
3. Update Forgejo SSH configuration
4. Test Git operations via new port
5. Document client configuration for users
6. Monitor performance and reliability

## References

- [NGINX Stream Module Documentation](http://nginx.org/en/docs/stream/ngx_stream_core_module.html)
- [HAProxy TCP Mode Configuration](https://www.haproxy.org/download/2.8/doc/configuration.txt)
- [Git SSH Configuration Best Practices](https://docs.github.com/en/authentication/connecting-to-github-with-ssh)
