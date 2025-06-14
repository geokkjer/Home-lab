# Packaging Claude Task Master AI for NixOS

This document outlines suggestions for packaging the "Claude Task Master AI" Node.js application as a Nix package. The typical installation method for this tool is `npm install -g task-master-ai`.

## 1. Creating the Nix Package

Nixpkgs provides helpers for packaging Node.js applications. The primary function for this is `buildNpmPackage`.

A Nix expression for this package can be created in your packages directory, for example, at `packages/claude-task-master-ai.nix`.

### Key Steps:

1.  **Find the Source**: Determine the source of the `task-master-ai` package. This is usually the npm registry. You'll need the package name and version.
2.  **Nix Expression (`default.nix` or `claude-task-master.nix`):** Create a Nix expression file.
3.  **Use `buildNpmPackage`**: This function handles the download, build, and installation of npm packages.
4.  **`npmDepsHash`**: You'll need to calculate a hash of the npm dependencies. This ensures reproducibility.
5.  **Binaries**: Ensure that the executables provided by `task-master-ai` are correctly placed in the output's `bin` directory. `buildNpmPackage` usually handles this if the `package.json` of the application specifies `bin` entries.

### Example Nix Expression:

```nix
{ lib, buildNpmPackage, fetchFromGitHub, nodejs }: # Add other dependencies if needed

buildNpmPackage rec {
  pname = "task-master-ai";
  version = "INSERT_PACKAGE_VERSION_HERE"; # Replace with the actual version

  src = fetchFromGitHub { # Or fetchurl if directly from npm/tarball
    owner = "eyaltoledano"; # Replace if this is not the correct source
    repo = "claude-task-master";  # Replace if this is not the correct source
    rev = "v${version}"; # Or specific commit/tag
    hash = "INSERT_SRC_HASH_HERE"; # lib.fakeSha256 for initial fetch, then replace
  };

  # If fetching directly from npm tarball:
  # src = fetchurl {
  #   url = "https://registry.npmjs.org/task-master-ai/-/task-master-ai-${version}.tgz";
  #   sha256 = "INSERT_TARBALL_HASH_HERE"; # lib.fakeSha256 for initial fetch, then replace
  # };

  npmDepsHash = "INSERT_NPMDEPSHASH_HERE"; # Calculate this after the first build attempt

  # buildInputs = [ nodejs ]; # buildNpmPackage usually brings in nodejs

  meta = with lib; {
    description = "Claude Task Master AI tool";
    homepage = "https://github.com/eyaltoledano/claude-task-master"; # Or actual homepage
    license = licenses.mit; # Check and replace with actual license
    maintainers = [ maintainers.yourGithubUsername ]; # Your username
    platforms = platforms.all;
  };
}
```

### Obtaining `npmDepsHash`:

1.  Initially, set `npmDepsHash = lib.fakeSha256;` or a placeholder like `"sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";`.
2.  Attempt to build the package (e.g., `nix-build -A task-master-ai`).
3.  The build will fail, but it will output the expected hash. Copy this hash into your Nix expression.
    Alternatively, you can use `prefetch-npm-deps` if you have a `package-lock.json`:
    ```sh
    # In a directory with package.json and package-lock.json for task-master-ai
    nix-shell -p nodePackages.prefetch-npm-deps --run "prefetch-npm-deps package-lock.json"
    ```
    Since `task-master-ai` is installed globally, you might need to fetch its source first to get the `package-lock.json`.

### Global Installation Aspect:

`buildNpmPackage` typically installs binaries specified in the `package.json`'s `bin` field into `$out/bin/`. This makes them available when the package is installed in a Nix profile. If `task-master-ai` is made available this way, VS Code can invoke it using `npx` as shown in the MCP server configuration, or potentially directly if it's added to the PATH.

## 2. Integrating with VS Code as an MCP Server

Instead of running `task-master-ai` as a system-wide NixOS service, it can be integrated directly into VS Code (or other compatible editors) as an MCP (Model Context Protocol) server. This allows your editor to communicate with the AI for task management capabilities.

The Nix package created in the previous step ensures that `task-master-ai` is available in your environment, typically invokable via `npx task-master-ai` or directly if the Nix package adds it to your PATH.

### VS Code `settings.json` Configuration:

You can configure VS Code to use `task-master-ai` as an MCP server by adding the following to your `settings.json` file:

```json
{
  "mcpServers": {
    "taskmaster-ai": {
      "command": "npx",
      "args": ["-y", "--package=task-master-ai", "task-master-ai"],
      "env": {
        "ANTHROPIC_API_KEY": "YOUR_ANTHROPIC_API_KEY_HERE",
        "PERPLEXITY_API_KEY": "YOUR_PERPLEXITY_API_KEY_HERE",
        "MODEL": "claude-3-7-sonnet-20250219",
        "PERPLEXITY_MODEL": "sonar-pro",
        "MAX_TOKENS": 64000,
        "TEMPERATURE": 0.2,
        "DEFAULT_SUBTASKS": 5,
        "DEFAULT_PRIORITY": "medium"
      }
    }
  }
}
```

**Key Points for MCP Configuration:**

*   **`command` and `args`**: These specify how to run `task-master-ai`. Using `npx -y --package=task-master-ai task-master-ai` ensures that `npx` fetches and runs the specified version of `task-master-ai`. If your Nix package makes `task-master-ai` directly available in the PATH, you might simplify the command to just `task-master-ai` and remove the `args` that specify the package for `npx`.
*   **`env`**: This section is crucial. You **must** replace placeholder API keys (`YOUR_ANTHROPIC_API_KEY_HERE`, `YOUR_PERPLEXITY_API_KEY_HERE`) with your actual keys.
*   You can customize other environment variables like `MODEL`, `MAX_TOKENS`, etc., according to your needs and the capabilities of `task-master-ai`.
*   Ensure the Nix package for `task-master-ai` (and `nodejs`/`npx`) is installed and accessible in the environment where VS Code runs.

## 3. Finding Package Information

*   **NPM Registry**: Search for `task-master-ai` on [npmjs.com](https://www.npmjs.com/) to find its exact version, dependencies, and potentially its source repository. The roadmap indicates the source is `https://github.com/eyaltoledano/claude-task-master.git`.
*   **GitHub Repository**: The roadmap points to `https://github.com/eyaltoledano/claude-task-master.git`. This is likely the best source for `package.json` and understanding how the tool works.

This guide provides a starting point. You'll need to adapt the examples based on the specifics of the `task-master-ai` tool.
