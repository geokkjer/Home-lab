# Home Manager Implementation TODO

Purpose
-------
This document is a concrete, actionable TODO for migrating user configuration from system-level NixOS modules to Home Manager. It translates the refactoring plan into step-by-step tasks, commands to run, tests to perform, and acceptance criteria. Use this as the single source of truth for the migration work and for PR checklists.

Assumptions & Prerequisites
---------------------------
- You have the repository root at `home-lab` and the current NixOS configuration in `machines/` and `flake.nix`.
- Current Emacs config (dotfiles and system packages) lives in `dotfiles/` and `modules/` as described in the refactor plan.
- You can create and edit `home-manager/` directory structure inside the repo.
- Test environment(s) available: a Development VM, a non-critical machine, and at least one critical machine for final validation.
- Familiarity with flakes, `nixos-rebuild --flake`, `home-manager` flakes integration, and systemd user services.

High-level Phases (mapped to PRs)
---------------------------------
- Phase 0 — Prep & Repo Hygiene (small PRs)
- Phase 1 — Home Manager Foundation (PR 1)
- Phase 2 — Emacs Migration (PR 2)
- Phase 3 — User Environment Completion (PR 3)
- Phase 4 — Multi-user & Profiles, Documentation (PR 4)
- Final — Cleanup & Reduce System Packages (PR 5)

Phase 0 — Prep & Repo Hygiene (preparatory)
------------------------------------------
Goal: make repo easy to modify, ensure backups, add issue/PR templates.

TODO:
- [ ] Add `home-manager/` directory to repo with `.gitkeep`.
- [ ] Ensure `dotfiles/` are complete and referenced in plan.
- [ ] Add migration tracking file `home-manager/MIGRATION_STATUS.md` to record progress per-machine and per-user.
- [ ] Add PR template with checklist items for:
  - compile/build flake, pass `nixos-rebuild` for a test machine,
  - run `home-manager switch` as user,
  - run basic user service start/stop tests,
  - rollback steps documented.
- [ ] Backup existing system-level Emacs and user dotfiles (tag or branch named `pre-home-manager-backup`).

Phase 1 — Home Manager Foundation
---------------------------------
Goal: Add Home Manager to flakes and wire up a minimal `home.nix` for `geir` without impacting system behavior.

Files to add:
- `home-manager/users/geir/home.nix`  (entry point)
- `home-manager/modules/`  (shared modules, initial empty)
- `home-manager/dotfiles/` (if not present already — optional copy of `dotfiles/` tree)
- Edit `flake.nix` to include `home-manager` input and reference `home-manager.nixosModules.home-manager` in `nixosConfigurations.<machine>.modules`.

Detailed TODO:
- [ ] Add `home-manager` flake input (follow pattern in plan). Example flake snippet to add or adapt:
  ```/dev/null/flake-sample.nix#L1-40
  {
    inputs = {
      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
      home-manager = {
        url = "github:nix-community/home-manager";
        inputs.nixpkgs.follows = "nixpkgs";
      };
    };
    outputs = { nixpkgs, home-manager, ... }: {
      nixosConfigurations = {
        my-host = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/my-host/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.geir = import ./home-manager/users/geir/home.nix;
            }
          ];
        };
      };
    };
  }
  ```
- [ ] Create a minimal `home.nix` for `geir` with `programs.bash` or `programs.zsh` disabled/set to not change behavior yet, but with `xdg.configFile` pointing to `home-manager/dotfiles` placeholders. Minimal file should set `home.username = "geir"`.
- [ ] Ensure `home-manager` integration is additive only: do not remove system-level equivalents yet. Use `home-manager.useGlobalPkgs = true` and `home-manager.useUserPackages = true` where appropriate to avoid package duplication.
- [ ] Run build: `sudo nixos-rebuild switch --flake .#my-host` (run first on Development VM).
- [ ] Verify system boots and existing system services unchanged.

Acceptance criteria:
- `nixos-rebuild` succeeds on a dev VM.
- `home-manager` module loads, and `home-manager.users.geir` is recognized.
- No change in user experience yet.

Phase 2 — Emacs Migration
-------------------------
Goal: Move Emacs configuration and the Emacs daemon to Home Manager while preserving the current profile-based system. This is the most delicate phase.

High-level plan:
1. Create a Home Manager Emacs program module under `home-manager/users/geir/programs/emacs.nix` (as in plan).
2. Move files from `/etc/emacs/` or system-managed location to `home-manager/dotfiles/emacs/`.
3. Implement user-level systemd service for Emacs (user service).
4. Test across all Emacs profiles (minimal/development/workstation/nox).

Detailed TODO:
- [ ] Create `home-manager/users/geir/programs/emacs.nix`. Include options for `programs.emacs.profile`, packaging, and `xdg.configFile` entries mapping to `dotfiles/emacs/`. Follow the example in the plan.
- [ ] Populate `home-manager/dotfiles/emacs/` with:
  - `init-nix.el` (or symlink from `dotfiles/emacs/`)
  - `modules/` (UI, completion, LSP integrations)
  - Any profile-specific config fragments
- [ ] Add `services.emacs` as a `systemd.user` service via Home Manager:
  - enable = true
  - package = `config.programs.emacs.package`
  - unit options for socket-activation if used
- [ ] Ensure `home.sessionVariables` contains `EMACS_PROFILE`, `RG_PATH`, `FD_PATH`, etc. (as in plan).
- [ ] Implement `xdg.configFile` map in `home.nix` to deploy Emacs files:
  - Example:
    ```/dev/null/emacs-xdg-sample.nix#L1-40
    xdg.configFile."emacs/init.el".source = ../../../dotfiles/emacs/init-nix.el;
    xdg.configFile."emacs/modules/ui.el".source = ../../../dotfiles/emacs/modules/ui.el;
    ```
- [ ] On dev VM:
  - Run `sudo nixos-rebuild switch --flake .#my-host` to ensure system picks up home-manager module.
  - As `geir`, run `home-manager switch --flake .#geir` (or the appropriate command if using `nixos-rebuild` way).
  - Start user service: `systemctl --user start emacs.service`.
  - Test Emacs startup in GUI and nox modes per profile.
- [ ] Validate all Emacs packages are available and that Nix tool integrations (LSP servers, formatters) still function.

Tests & Verification:
- [ ] Emacs loads the same modules and packages as before in each profile.
- [ ] Emacs daemon starts/stops via `systemctl --user`.
- [ ] `M-x` package functionality and LSP are okay.
- [ ] Emacs performance (startup) comparable.

Rollout strategy for Emacs:
- Start with dev VM.
- Then test on non-critical workstation.
- Finally, move to critical machines after 1–2 days of stable testing.

Phase 3 — Complete User Migration (shell, dev tools, desktop)
-------------------------------------------------------------
Goal: Migrate zsh, tmux, starship, git configs, development tooling, and desktop-related user services.

Detailed TODO:
- Shell
  - [ ] Move `.zshrc`, `.zprofile`, `.zshenv` into `home.file` entries or `xdg.configFile` as appropriate.
  - [ ] Migrate `oh-my-zsh` or plugin setups to nix-managed packages and reference them in `home.packages`.
  - [ ] Add aliases, functions, and environment variables via `home.shellInit` or `home.sessionVariables`.
- Tmux / starship / git
  - [ ] Add `home.file." .tmux.conf".source` entries for tmux.
  - [ ] Add `xdg.configFile."starship.toml".text` or `.source`.
  - [ ] Move `git/config` to `xdg.configFile."git/config"`.
- Development environments
  - [ ] Migrate language tooling into `home.packages` and `programs.*` options:
    - LSP clients, `direnv`, `asdf` (if used), `nix-direnv`.
  - [ ] Add language-server config files to `xdg.configFile` as needed (e.g., `~/.config/language-servers/`).
  - [ ] If using `lorri` or `direnv`, ensure integration preserved.
- Desktop integration
  - [ ] Create user-level systemd units for desktop services (`desktop-notifier.service`, `pipewire` user-level bits if relevant).
  - [ ] Move application config files to `xdg.configFile`.
- Tests:
  - [ ] Login shell behaves identically.
  - [ ] Starship prompt displays correctly.
  - [ ] tmux sessions restore/behave identical.
  - [ ] Development tooling loads and LSP functions in editors.
  - [ ] Desktop services start as expected.

Phase 4 — Multi-user, Profiles, and Documentation
-------------------------------------------------
Goal: Add `sma` user, create shared modules, and a flexible profile switching mechanism.

Detailed TODO:
- [ ] Add `home-manager/users/sma/home.nix` modeled from `geir` with appropriate customizations.
- [ ] Create `home-manager/modules/` for reusable modules: `emacs/`, `development/`, `desktop/`.
- [ ] Implement profile selection mechanism for `geir`:
  - Example approach: `home-manager` options `programs.emacs.profiles` and `home.packages.profile` and set via `home.profile = "development"`.
- [ ] Document how to switch profiles and how to add a new profile.
- [ ] Create end-user documentation pages:
  - `docs/home-manager/README.md` — high-level overview and how to use.
  - `docs/home-manager/MIGRATION_GUIDE.md` — step-by-step rollback and fix recipes.
  - `docs/home-manager/TRIAGE.md` — common breakages and recovery commands.
- [ ] Add CI checks (if repo has CI) to:
  - Validate flake builds: `nix flake check` or `nix build`.
  - Optionally run `nix eval` to ensure flake outputs include `nixosConfigurations` and `home-manager` entries.

Phase 5 — Reduce System Packages / Cleanup
------------------------------------------
Goal: Move packages from `environment.systemPackages` to `home.packages` where appropriate and remove old system-level config.

TODO:
- [ ] Inventory packages currently in `environment.systemPackages` that are user-specific.
  - [ ] Create a migration spreadsheet (or `PACKAGES_TO_MOVE.md`) with entries: package, reason, user, notes.
- [ ] For each candidate package:
  - [ ] Move to `home.packages` in `home.nix`.
  - [ ] Run `nixos-rebuild switch` then `home-manager switch`.
  - [ ] Verify functionality.
- [ ] Once all user packages moved and validated, remove them from `configuration.nix` and `flake.nix` system module.
- [ ] Remove duplicated config files left in system-level `/etc` when confirmed not needed.
- [ ] Final PR to delete now-unused system-level modules.

Rollback & Emergency Procedures
-------------------------------
- Emergency rollback for a machine:
  - [ ] `git checkout <pre-home-manager-backup-branch>` and push/merge revert.
  - [ ] `sudo nixos-rebuild switch --flake .#<machine>` to restore system-level config.
- Home Manager-specific rollback:
  - [ ] Restore `home.nix` from previous commit.
  - [ ] As user: `home-manager switch` with previous config commit.
  - [ ] Stop user services: `systemctl --user stop <service>` or `systemctl --user disable --now <service>`.
- Troubleshooting aids to document in `TRIAGE.md`:
  - `journalctl --user -b -u <service>`
  - `home-manager switch --show-trace` 
  - `systemctl --user status <service>`
  - `nixos-rebuild build --flake .#<machine>` + `result/activate` (test without switching)

Testing Matrix & Acceptance Criteria
-----------------------------------
Per-item tests to run for each machine and user:

Basic:
- [ ] `sudo nixos-rebuild switch --flake .#<machine>` succeeds (no rebuild regressions).
- [ ] `home-manager switch` for the user succeeds with no errors.
- [ ] No regression in login shells, desktop, or critical services.

Emacs:
- [ ] All Emacs profiles start and behave identically.
- [ ] LSP servers connect and work.
- [ ] Packages available and configured.

Services:
- [ ] User services start/stop via `systemctl --user`.
- [ ] Socket-activated services bind properly.

Dotfiles:
- [ ] Config files deployed to `~/.config/` or `~/` match previous state where required.
- [ ] No duplicate/conflicting files remain.

Multi-user:
- [ ] `sma` user config applies and behaves as intended.
- [ ] Shared modules work across users.

Performance:
- [ ] Home Manager switch time < 30s for standard updates (target).
- [ ] Emacs startup time not degraded.

PR Checklist Template
---------------------
Use this for each PR performing migration work:

- [ ] Flake builds locally: `nix build .#nixosConfigurations.<machine>.config.system.build.toplevel`
- [ ] NixOS rebuild tested on Dev VM.
- [ ] `home-manager switch` successful and idempotent.
- [ ] User services started and tested.
- [ ] Dotfiles present and validated.
- [ ] Rollback steps documented in PR description.
- [ ] Tests listed in PR summary passed manually (or via CI).

Small Helpful Command Snippets
------------------------------
- Build flake system:
  ```/dev/null/commands.sh#L1-12
  # sudo nixos-rebuild switch --flake .#my-host
  sudo nixos-rebuild switch --flake .#my-host
  ```
- Switch Home Manager for user (if using `home-manager` flake integration):
  ```/dev/null/commands.sh#L13-24
  # As user 'geir':
  home-manager switch --flake /path/to/repo#geir
  # or, if using system-managed home-manager:
  sudo nixos-rebuild switch --flake .#my-host
  ```
- Check user service logs:
  ```/dev/null/commands.sh#L25-40
  # As user:
  systemctl --user status emacs.service
  journalctl --user -u emacs.service --no-pager -n 200
  ```

Owners & Time Estimates
-----------------------
- Phase 0: Repo owner / maintainer — 1-2 days
- Phase 1: Integrator — 2 days
- Phase 2: Emacs lead (you or delegated) — 1 week
- Phase 3: Shell & dev tooling owner — 1 week
- Phase 4: Multi-user & docs — 1 week
- Phase 5: Cleanup — 3 days

Notes & Risk Mitigation
-----------------------
- Emacs migration is highest risk — keep parallel working system and test thoroughly before removing system-managed Emacs.
- Keep system-level packages for a buffer period to reduce urgency of rollback.
- Make incremental PRs, one functional area at a time (foundation -> emacs -> shell -> dev -> desktop -> cleanup).
- Keep documented rollback steps inside each PR so reviewers can test revert easily.

Appendix - Useful Reference Paths
--------------------------------
- Planned home-manager tree:
  - `home-manager/users/geir/home.nix`
  - `home-manager/users/geir/programs/emacs.nix`
  - `home-manager/modules/emacs/`
  - `home-manager/dotfiles/emacs/`
- Existing repository items that will be consulted:
  - `flake.nix`
  - `machines/<host>/configuration.nix`
  - `dotfiles/` (current configs)

End of TODO
-----------
Follow this checklist and break the work into the suggested PRs. If you want, I can convert phases into individual GitHub issues with descriptions, commands, and checkboxes for each subtask.