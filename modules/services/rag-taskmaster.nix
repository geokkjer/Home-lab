{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.homelab-rag-taskmaster;

  # Python environment with all RAG and MCP dependencies
  ragPython = pkgs.python3.withPackages (ps:
    with ps; [
      # Core RAG dependencies
      langchain
      langchain-community
      langchain-chroma
      chromadb
      sentence-transformers

      # MCP dependencies
      fastapi
      uvicorn
      pydantic
      aiohttp

      # Additional utilities
      unstructured
      markdown
      requests
      numpy

      # Custom MCP package (would need to be built)
      # (ps.buildPythonPackage rec {
      #   pname = "mcp";
      #   version = "1.0.0";
      #   src = ps.fetchPypi {
      #     inherit pname version;
      #     sha256 = "0000000000000000000000000000000000000000000000000000";
      #   };
      #   propagatedBuildInputs = with ps; [ pydantic aiohttp ];
      # })
    ]);

  # Node.js environment for Task Master
  nodeEnv = pkgs.nodejs_20;

  # Service configuration files
  ragConfigFile = pkgs.writeText "rag-config.json" (builtins.toJSON {
    ollama_base_url = "http://localhost:11434";
    vector_store_path = "${cfg.dataDir}/chroma_db";
    docs_path = cfg.docsPath;
    chunk_size = cfg.chunkSize;
    chunk_overlap = cfg.chunkOverlap;
    max_retrieval_docs = cfg.maxRetrievalDocs;
  });

  taskMasterConfigFile = pkgs.writeText "taskmaster-config.json" (builtins.toJSON {
    taskmaster_path = "${cfg.dataDir}/taskmaster";
    ollama_base_url = "http://localhost:11434";
    default_model = "llama3.3:8b";
    project_templates = cfg.projectTemplates;
  });
in {
  options.services.homelab-rag-taskmaster = {
    enable = mkEnableOption "Home Lab RAG + Task Master AI Integration";

    # Basic configuration
    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/rag-taskmaster";
      description = "Directory for RAG and Task Master data";
    };

    docsPath = mkOption {
      type = types.path;
      default = "/home/geir/Home-lab";
      description = "Path to documentation to index";
    };

    # Port configuration
    ragPort = mkOption {
      type = types.port;
      default = 8080;
      description = "Port for RAG API service";
    };

    mcpRagPort = mkOption {
      type = types.port;
      default = 8081;
      description = "Port for RAG MCP server";
    };

    mcpTaskMasterPort = mkOption {
      type = types.port;
      default = 8082;
      description = "Port for Task Master MCP bridge";
    };

    # RAG configuration
    chunkSize = mkOption {
      type = types.int;
      default = 1000;
      description = "Size of document chunks for embedding";
    };

    chunkOverlap = mkOption {
      type = types.int;
      default = 200;
      description = "Overlap between document chunks";
    };

    maxRetrievalDocs = mkOption {
      type = types.int;
      default = 5;
      description = "Maximum number of documents to retrieve for RAG";
    };

    embeddingModel = mkOption {
      type = types.str;
      default = "all-MiniLM-L6-v2";
      description = "Sentence transformer model for embeddings";
    };

    # Task Master configuration
    enableTaskMaster = mkOption {
      type = types.bool;
      default = true;
      description = "Enable Task Master AI integration";
    };

    projectTemplates = mkOption {
      type = types.listOf types.str;
      default = [
        "fullstack-web-app"
        "nixos-service"
        "home-lab-tool"
        "api-service"
        "frontend-app"
      ];
      description = "Available project templates for Task Master";
    };

    # Update configuration
    updateInterval = mkOption {
      type = types.str;
      default = "1h";
      description = "How often to update the document index";
    };

    autoUpdateDocs = mkOption {
      type = types.bool;
      default = true;
      description = "Automatically update document index when files change";
    };

    # Security configuration
    enableAuth = mkOption {
      type = types.bool;
      default = false;
      description = "Enable authentication for API access";
    };

    allowedUsers = mkOption {
      type = types.listOf types.str;
      default = ["geir"];
      description = "Users allowed to access the services";
    };

    # Monitoring configuration
    enableMetrics = mkOption {
      type = types.bool;
      default = true;
      description = "Enable Prometheus metrics collection";
    };

    metricsPort = mkOption {
      type = types.port;
      default = 9090;
      description = "Port for Prometheus metrics";
    };
  };

  config = mkIf cfg.enable {
    # Ensure required system packages
    environment.systemPackages = with pkgs; [
      nodeEnv
      ragPython
      git
    ];

    # Create system user and group
    users.users.rag-taskmaster = {
      isSystemUser = true;
      group = "rag-taskmaster";
      home = cfg.dataDir;
      createHome = true;
      description = "RAG + Task Master AI service user";
    };

    users.groups.rag-taskmaster = {};

    # Ensure data directories exist
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0755 rag-taskmaster rag-taskmaster -"
      "d ${cfg.dataDir}/chroma_db 0755 rag-taskmaster rag-taskmaster -"
      "d ${cfg.dataDir}/taskmaster 0755 rag-taskmaster rag-taskmaster -"
      "d ${cfg.dataDir}/logs 0755 rag-taskmaster rag-taskmaster -"
      "d ${cfg.dataDir}/cache 0755 rag-taskmaster rag-taskmaster -"
    ];

    # Core RAG service
    systemd.services.homelab-rag = {
      description = "Home Lab RAG Service";
      wantedBy = ["multi-user.target"];
      after = ["network.target" "ollama.service"];
      wants = ["ollama.service"];

      serviceConfig = {
        Type = "simple";
        User = "rag-taskmaster";
        Group = "rag-taskmaster";
        WorkingDirectory = cfg.dataDir;
        ExecStart = "${ragPython}/bin/python -m rag_service --config ${ragConfigFile}";
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        Restart = "always";
        RestartSec = 10;

        # Security settings
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [cfg.dataDir];
        ReadOnlyPaths = [cfg.docsPath];

        # Resource limits
        MemoryMax = "4G";
        CPUQuota = "200%";
      };

      environment = {
        PYTHONPATH = "${ragPython}/${ragPython.sitePackages}";
        OLLAMA_BASE_URL = "http://localhost:11434";
        VECTOR_STORE_PATH = "${cfg.dataDir}/chroma_db";
        DOCS_PATH = cfg.docsPath;
        LOG_LEVEL = "INFO";
      };
    };

    # RAG MCP Server
    systemd.services.homelab-rag-mcp = {
      description = "Home Lab RAG MCP Server";
      wantedBy = ["multi-user.target"];
      after = ["network.target" "homelab-rag.service"];
      wants = ["homelab-rag.service"];

      serviceConfig = {
        Type = "simple";
        User = "rag-taskmaster";
        Group = "rag-taskmaster";
        WorkingDirectory = cfg.dataDir;
        ExecStart = "${ragPython}/bin/python -m mcp_rag_server --config ${ragConfigFile}";
        Restart = "always";
        RestartSec = 10;

        # Security settings
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [cfg.dataDir];
        ReadOnlyPaths = [cfg.docsPath];
      };

      environment = {
        PYTHONPATH = "${ragPython}/${ragPython.sitePackages}";
        OLLAMA_BASE_URL = "http://localhost:11434";
        VECTOR_STORE_PATH = "${cfg.dataDir}/chroma_db";
        DOCS_PATH = cfg.docsPath;
        MCP_PORT = toString cfg.mcpRagPort;
      };
    };

    # Task Master setup service (runs once to initialize)
    systemd.services.homelab-taskmaster-setup = mkIf cfg.enableTaskMaster {
      description = "Task Master AI Setup";
      after = ["network.target"];
      wantedBy = ["multi-user.target"];

      serviceConfig = {
        Type = "oneshot";
        User = "rag-taskmaster";
        Group = "rag-taskmaster";
        WorkingDirectory = "${cfg.dataDir}/taskmaster";
        RemainAfterExit = true;
      };

      environment = {
        NODE_ENV = "production";
        PATH = "${nodeEnv}/bin:${pkgs.git}/bin";
      };

      script = ''
        # Clone Task Master if not exists
        if [ ! -d "${cfg.dataDir}/taskmaster/.git" ]; then
          ${pkgs.git}/bin/git clone https://github.com/eyaltoledano/claude-task-master.git ${cfg.dataDir}/taskmaster
          cd ${cfg.dataDir}/taskmaster
          ${nodeEnv}/bin/npm install

          # Initialize with home lab configuration
          ${nodeEnv}/bin/npx task-master init --yes \
            --name "Home Lab Development" \
            --description "NixOS-based home lab and fullstack development projects" \
            --author "Geir" \
            --version "1.0.0"
        fi

        # Ensure proper permissions
        chown -R rag-taskmaster:rag-taskmaster ${cfg.dataDir}/taskmaster
      '';
    };

    # Task Master MCP Bridge
    systemd.services.homelab-taskmaster-mcp = mkIf cfg.enableTaskMaster {
      description = "Task Master MCP Bridge";
      wantedBy = ["multi-user.target"];
      after = ["network.target" "homelab-taskmaster-setup.service" "homelab-rag.service"];
      wants = ["homelab-taskmaster-setup.service" "homelab-rag.service"];

      serviceConfig = {
        Type = "simple";
        User = "rag-taskmaster";
        Group = "rag-taskmaster";
        WorkingDirectory = "${cfg.dataDir}/taskmaster";
        ExecStart = "${ragPython}/bin/python -m mcp_taskmaster_bridge --config ${taskMasterConfigFile}";
        Restart = "always";
        RestartSec = 10;

        # Security settings
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [cfg.dataDir];
        ReadOnlyPaths = [cfg.docsPath];
      };

      environment = {
        PYTHONPATH = "${ragPython}/${ragPython.sitePackages}";
        NODE_ENV = "production";
        PATH = "${nodeEnv}/bin:${pkgs.git}/bin";
        OLLAMA_BASE_URL = "http://localhost:11434";
        TASKMASTER_PATH = "${cfg.dataDir}/taskmaster";
        MCP_PORT = toString cfg.mcpTaskMasterPort;
      };
    };

    # Document indexing service (periodic update)
    systemd.services.homelab-rag-indexer = mkIf cfg.autoUpdateDocs {
      description = "Home Lab RAG Document Indexer";

      serviceConfig = {
        Type = "oneshot";
        User = "rag-taskmaster";
        Group = "rag-taskmaster";
        WorkingDirectory = cfg.dataDir;
        ExecStart = "${ragPython}/bin/python -m rag_indexer --config ${ragConfigFile} --update";
      };

      environment = {
        PYTHONPATH = "${ragPython}/${ragPython.sitePackages}";
        DOCS_PATH = cfg.docsPath;
        VECTOR_STORE_PATH = "${cfg.dataDir}/chroma_db";
      };
    };

    # Timer for periodic document updates
    systemd.timers.homelab-rag-indexer = mkIf cfg.autoUpdateDocs {
      description = "Periodic RAG document indexing";
      wantedBy = ["timers.target"];

      timerConfig = {
        OnBootSec = "5m";
        OnUnitActiveSec = cfg.updateInterval;
        Unit = "homelab-rag-indexer.service";
      };
    };

    # Prometheus metrics exporter (if enabled)
    systemd.services.homelab-rag-metrics = mkIf cfg.enableMetrics {
      description = "RAG + Task Master Metrics Exporter";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];

      serviceConfig = {
        Type = "simple";
        User = "rag-taskmaster";
        Group = "rag-taskmaster";
        WorkingDirectory = cfg.dataDir;
        ExecStart = "${ragPython}/bin/python -m metrics_exporter --port ${toString cfg.metricsPort}";
        Restart = "always";
        RestartSec = 10;
      };

      environment = {
        PYTHONPATH = "${ragPython}/${ragPython.sitePackages}";
        METRICS_PORT = toString cfg.metricsPort;
        RAG_SERVICE_URL = "http://localhost:${toString cfg.ragPort}";
      };
    };

    # Firewall configuration
    networking.firewall.allowedTCPPorts =
      mkIf (!cfg.enableAuth) [
        cfg.ragPort
        cfg.mcpRagPort
        cfg.mcpTaskMasterPort
      ]
      ++ optionals cfg.enableMetrics [cfg.metricsPort];

    # Nginx reverse proxy configuration (optional)
    services.nginx.virtualHosts."rag.home.lab" = mkIf config.services.nginx.enable {
      listen = [
        {
          addr = "0.0.0.0";
          port = 80;
        }
        {
          addr = "0.0.0.0";
          port = 443;
          ssl = true;
        }
      ];

      locations = {
        "/api/rag/" = {
          proxyPass = "http://localhost:${toString cfg.ragPort}/";
          proxyWebsockets = true;
        };

        "/api/mcp/rag/" = {
          proxyPass = "http://localhost:${toString cfg.mcpRagPort}/";
          proxyWebsockets = true;
        };

        "/api/mcp/taskmaster/" = mkIf cfg.enableTaskMaster {
          proxyPass = "http://localhost:${toString cfg.mcpTaskMasterPort}/";
          proxyWebsockets = true;
        };

        "/metrics" = mkIf cfg.enableMetrics {
          proxyPass = "http://localhost:${toString cfg.metricsPort}/";
        };
      };

      # SSL configuration would go here if needed
      # sslCertificate = "/path/to/cert";
      # sslCertificateKey = "/path/to/key";
    };
  };
}
