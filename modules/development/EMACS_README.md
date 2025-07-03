# Emacs + Nix Setup: Overview and Package Guide

## Introduction

This document explains the Emacs ecosystem and the Nix-based configuration used in this project. It covers the purpose of key Emacs packages, how they are managed with Nix, and how to extend or troubleshoot your setup.

---

## Why Use Nix for Emacs?

- **Reproducibility:** Nix ensures your Emacs environment is consistent across machines and rebuilds.
- **Declarative Configuration:** All packages and dependencies are listed in a single place (`emacs.nix`), making it easy to update or share.
- **Isolation:** Nix prevents conflicts between Emacs packages and system/global packages.

---

## Emacs Packages in This Setup

### Navigation & Completion

- **vertico, consult, marginalia, orderless, embark, embark-consult, corfu, cape:** Modern completion and minibuffer enhancements for fast, fuzzy, and context-aware navigation.
- **projectile:** Project management and navigation.

### Version Control

- **magit:** The best Git interface for Emacs.
- **forge:** GitHub/GitLab/Magit integration for issues and pull requests.

### Development Tools

- **lsp-mode, lsp-ui:** Language Server Protocol support for IDE-like features.
- **company:** In-buffer code completion.
- **flycheck:** On-the-fly syntax checking.
- **yasnippet:** Snippet expansion for faster coding.
- **elisp-slime-nav:** Easy navigation of Emacs Lisp code.
- **aggressive-indent:** Keeps your code automatically indented.
- **highlight-defined:** Highlights defined symbols in Emacs Lisp.

### Language Support

- **nix-mode:** Nix language editing.
- **rust-mode, python-mode, typescript-mode, json-mode, yaml-mode, markdown-mode:** Major modes for popular languages and formats.
- **elisp-ls:** Emacs Lisp Language Server for LSP features in Emacs Lisp.

### Org & Knowledge Management

- **org, org-roam, org-roam-ui:** Org-mode for notes, tasks, and literate programming; org-roam for Zettelkasten-style knowledge management.

### UI & Usability

- **doom-themes, doom-modeline, all-the-icons, rainbow-delimiters, highlight-indent-guides:** Visual enhancements for a modern, readable Emacs.
- **smartparens, expand-region, multiple-cursors, avy, ace-window:** Editing and navigation power tools.
- **vterm:** Terminal emulator inside Emacs.

---

## How the Nix Setup Works

- The file `modules/development/emacs.nix` defines a function that builds a custom Emacs with all the above packages.
- Packages are grouped by purpose (essential, minimal, development, workstation) and selected based on your machine profile.
- When you rebuild your system or run `nix develop`, Nix fetches and builds all required Emacs packages.
- Environment variables (like `AG_PATH`, `FD_PATH`, etc.) are set for Emacs to use external tools.

---

## Extending Your Setup

- To add a new package, add it to the appropriate list in `emacs.nix` (e.g., under `development = epkgs: with epkgs; [ ... ]`).
- Rebuild your system or development shell to apply changes.
- For packages not in Nixpkgs, consider using MELPA as a fallback, but prefer Nix for reproducibility.

---

## Troubleshooting

- If you see `Cannot open load file` errors, make sure the package is included in your Nix config.
- For missing language servers, check if the correct Nix package is used (e.g., `elisp-ls` for Emacs Lisp LSP).
- Use `emacs --daemon --debug-init` to debug startup issues.

---

## Resources

- [NixOS Emacs Wiki](https://nixos.wiki/wiki/Emacs)
- [Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Nixpkgs Emacs Packages](https://search.nixos.org/packages?channel=unstable&show=emacsPackages)
- [System Crafters: Emacs from Scratch](https://systemcrafters.net/)

---

This setup gives you a modern, reproducible, and powerful Emacs environment managed entirely by Nix. Happy hacking!
