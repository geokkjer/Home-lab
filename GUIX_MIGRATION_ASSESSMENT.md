# Viability Assessment: Migrating from NixOS to a Guix System with Nix Integration

## 1. Executive Summary

**Conclusion: Viable, but with significant added complexity.**

Migrating the current NixOS-based infrastructure to a Guix System base is technically feasible. It is possible to install the Nix package manager on top of Guix System to retain access to `nix` development environments and the larger Nixpkgs software repository.

However, this approach introduces a "hybrid" model that requires managing two distinct declarative systems. The primary benefit is gaining access to Guix's Scheme-based configuration and its free-software-oriented ecosystem while not losing the breadth of the Nixpkgs collection. The main drawback is the cognitive overhead and maintenance burden of running two parallel package management ecosystems.

This path is recommended only if the core team is comfortable with the intricacies of both Guix and Nix and is prepared for the long-term maintenance of this dual-system setup. A phased migration, starting with a single, non-critical machine, is strongly advised.

## 2. Goal

The objective is to migrate the current "pure" NixOS setup to a base operating system managed by GNU Guix (Guix System). A key requirement is to continue using Nix for:
1.  Creating isolated development environments (i.e., `nix develop`/`nix-shell`).
2.  Installing additional software that may not be available or is preferred from the Nixpkgs repository.

## 3. Core Concepts: The Hybrid Approach

The proposed setup would function as follows:

*   **Base Operating System:** Guix System would manage the kernel, system services (like `shepherd`, networking, user accounts), and the base set of system-wide packages. The entire system configuration would be defined declaratively in Guile Scheme (`.scm` files).
*   **Nix as a "Second-Level" Package Manager:** The Nix package manager would be installed on top of the running Guix System. Guix System has a dedicated service (`nix-service-type`) to facilitate this, which properly runs the `nix-daemon`.
*   **Package Segregation:**
    *   **Guix:** Manages the core OS and system-level dependencies.
    *   **Nix:** Manages user-specific applications and, crucially, the project-specific development shells. These packages would live within the Nix store (`/nix/store`) and would be managed independently of Guix's store (`/gnu/store`).

## 4. Viability Analysis

### 4.1. Technical Feasibility

Research confirms that this is a well-trodden path. The Guix community provides the necessary `nix-service-type` to ensure that the Nix daemon runs correctly within a Guix System. Users can then install and use Nix commands (`nix-env`, `nix-shell`, `nix develop`) as they would on any other Linux distribution.

### 4.2. Benefits

*   **Access to Both Ecosystems:** The primary driver for this approach. It allows the use of Guix's elegant Scheme-based configuration and its strong commitment to free software, while simultaneously providing a practical escape hatch to the much larger and more comprehensive Nixpkgs repository for proprietary software, niche packages, or bleeding-edge versions.
*   **Retain Familiar Dev Environments:** Developers can continue using their existing `flake.nix` or `shell.nix` files without modification, preserving established workflows.
*   **Philosophical Alignment:** Allows for a base system that aligns with the GNU philosophy (Guix) while pragmatically using non-free software where necessary (via Nix).

### 4.3. Challenges & Risks

*   **Increased Complexity:** This is the most significant risk. The team will need to be proficient in both Nix and Guix. Debugging issues may become more complex, as it might not be immediately clear whether a problem originates from the Guix base system or the Nix overlay.
*   **Configuration Drift:** There is a risk of configuration being split illogically between the two systems. A clear policy would be needed to define what should be managed by Guix (the base system) versus Nix (user-level applications, dev environments).
*   **Two Sources of Truth:** You will have two separate, declarative package management systems. This means two sets of package definitions, two update mechanisms (`guix pull` and `nix-channel --update` or `nix flake update`), and two stores (`/gnu/store` and `/nix/store`).
*   **Potential for Conflicts:** While both systems are designed for isolation, subtle conflicts could arise with environment variables, system paths, or service ports if not managed carefully. For example, ensuring that binaries from the Nix profile have precedence over Guix-installed ones (or vice-versa) requires careful shell configuration.

## 5. High-Level Migration Plan

A gradual migration is essential to mitigate risks.

1.  **Sandbox Machine:** Designate a test machine (or a VM) for the initial migration. Do not attempt this on a critical machine like `sleeper-service` first.
2.  **Guix Base Install:** Perform a fresh installation of Guix System on the sandbox machine.
3.  **Replicate Core Services:** Begin porting the existing NixOS configuration for a single machine to a Guix `config.scm`. Focus on the absolute essentials: networking, user accounts, SSH access.
4.  **Enable Nix Service:** Add the `nix-service-type` to the Guix configuration to install and run the Nix daemon. Reconfigure the system.
5.  **Install Nix and Port Dev Environments:** As a user, install the Nix package manager. Copy over an existing project with a `flake.nix` and test the `nix develop` workflow.
6.  **Port User-Level Packages:** Instead of porting a `home-manager` configuration directly, decide on a strategy. Either:
    *   Replicate the package list in Guix's `home-environment`.
    *   Install them using `nix-env` and manage them via a Nix-based home-manager setup running on top of Guix.
7.  **Assess and Document:** Thoroughly test the sandbox system. Document all challenges, solutions, and the final configuration strategy.
8.  **Phased Rollout:** Based on the success of the sandbox experiment, create a detailed plan for migrating other machines one by one.
