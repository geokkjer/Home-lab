# TidalCycles Emacs Configuration

Copy the relevant sections to your `init.el` or literate config (e.g., `Emacs.org`).

## Prerequisites (Arch Linux / CachyOS)

Arch Linux has **native TidalCycles packages** - no cabal compilation needed!

```bash
# Install TidalCycles and all Haskell dependencies via pacman
sudo pacman -Sy ghc ghc-libs haskell-{tidal,bifunctors,colour,hosc,mwc-random,network,primitive,random,vector,microspec}

# Install SuperCollider
sudo pacman -S supercollider sc3-plugins

# Download BootTidal.hs
mkdir -p ~/.local/share/SuperCollider
wget https://raw.githubusercontent.com/tidalcycles/Tidal/master/BootTidal.hs \
  -O ~/.local/share/SuperCollider/BootTidal.hs

# Verify
ghci --version
ls ~/.local/share/SuperCollider/BootTidal.hs
```

Then install SuperDirt in SuperCollider (run in `sclang`):

```supercollider
Quarks.checkForUpdates({Quarks.install("SuperDirt", "v1.7.4"); thisProcess.recompile()})
```

---

## Minimal Configuration

For a quick start with sensible defaults:

```elisp
;;; TidalCycles - Live Coding Environment
(use-package tidal
  :ensure t
  :mode ("\\.tidal\\'" . tidal-mode)
  :custom
  (tidal-interpreter "ghci")
  ;; Arch Linux: BootTidal.hs downloaded to this location
  (tidal-boot-script-path "~/.local/share/SuperCollider/BootTidal.hs"))
```

---

## Standard Configuration (Recommended)

Auto-finds BootTidal.hs and adds useful keybindings:

```elisp
;;; TidalCycles - Live Coding Environment
;;;
;;; Prerequisites (Arch Linux):
;;;   - sudo pacman -S ghc haskell-tidal supercollider sc3-plugins
;;;   - Download BootTidal.hs to ~/.local/share/SuperCollider/
;;;   - Install SuperDirt quark in SuperCollider
;;;   - Start SuperDirt before using Tidal

(use-package tidal
  :ensure t
  :mode ("\\.tidal\\'" . tidal-mode)
  :custom
  (tidal-interpreter "ghci")
  ;; Search common locations for BootTidal.hs
  (tidal-boot-script-path
   (or (and (file-exists-p "~/.local/share/SuperCollider/BootTidal.hs")
            "~/.local/share/SuperCollider/BootTidal.hs")
       (car (file-expand-wildcards "~/.cabal/share/tidal-*/BootTidal.hs"))
       "~/.local/share/SuperCollider/BootTidal.hs"))
  
  :config
  ;; Visual feedback when hushing
  (defun my/tidal-hush-notify ()
    "Hush all patterns with visual notification."
    (interactive)
    (tidal-hush)
    (message "🔇 All patterns silenced"))
  
  :bind (:map tidal-mode-map
         ("C-c C-s" . tidal-run-line)          ; Evaluate line
         ("C-c C-e" . tidal-run-multiple-lines) ; Evaluate block
         ("C-c C-r" . tidal-run-region)         ; Evaluate selection
         ("C-c C-h" . my/tidal-hush-notify)     ; Silence all
         ("C-<return>" . tidal-run-line)))      ; Quick eval
```

---

## Full Configuration (with Helper Functions)

Complete setup with startup helpers and custom functions:

```elisp
;;; ==========================================================================
;;; TidalCycles - Algorithmic Pattern Live Coding
;;; ==========================================================================
;;;
;;; Installation (Arch Linux / CachyOS):
;;;   sudo pacman -Sy ghc ghc-libs haskell-{tidal,bifunctors,colour,hosc,mwc-random,network,primitive,random,vector,microspec}
;;;   sudo pacman -S supercollider sc3-plugins
;;;   wget https://raw.githubusercontent.com/tidalcycles/Tidal/master/BootTidal.hs -O ~/.local/share/SuperCollider/BootTidal.hs
;;;
;;; Workflow:
;;;   1. Start SuperCollider and run: SuperDirt.start
;;;   2. Open a .tidal file in Emacs
;;;   3. M-x tidal-start-haskell (or C-c C-s after this config)
;;;   4. Write patterns and evaluate with C-<return>
;;;   5. C-c C-h to silence everything

(use-package tidal
  :ensure t
  :mode ("\\.tidal\\'" . tidal-mode)
  
  :custom
  (tidal-interpreter "ghci")
  ;; Arch Linux: pacman installs to system, BootTidal.hs downloaded manually
  ;; Also checks cabal path for other distros
  (tidal-boot-script-path
   (or (and (file-exists-p "~/.local/share/SuperCollider/BootTidal.hs")
            "~/.local/share/SuperCollider/BootTidal.hs")
       (car (file-expand-wildcards "~/.cabal/share/tidal-*/BootTidal.hs"))
       (car (file-expand-wildcards "/nix/store/*-tidal-*/share/BootTidal.hs"))
       "~/.local/share/SuperCollider/BootTidal.hs"))
  
  :config
  ;; --- Helper Functions ---
  
  (defun my/tidal-hush-notify ()
    "Hush all patterns with visual notification."
    (interactive)
    (tidal-hush)
    (message "🔇 All patterns silenced"))
  
  (defun my/tidal-run-block-notify ()
    "Run current block with feedback."
    (interactive)
    (tidal-run-multiple-lines)
    (message "▶ Pattern sent"))
  
  (defun my/tidal-solo (n)
    "Solo pattern N, muting all others."
    (interactive "nSolo pattern number (1-9): ")
    (let ((cmd (format "solo %d" n)))
      (tidal-send-string cmd)
      (message "🎯 Solo: d%d" n)))
  
  (defun my/tidal-unsolo ()
    "Unsolo - restore all patterns."
    (interactive)
    (tidal-send-string "unsolo")
    (message "🔊 All patterns restored"))
  
  (defun my/tidal-insert-template ()
    "Insert a basic Tidal pattern template."
    (interactive)
    (insert "-- Pattern block\nd1 $ sound \"bd sn\"\n\nd2 $ sound \"hh*8\" # gain 0.7\n\n-- hush\n"))
  
  :bind (:map tidal-mode-map
         ;; Evaluation
         ("C-c C-s" . tidal-run-line)
         ("C-c C-e" . my/tidal-run-block-notify)
         ("C-c C-r" . tidal-run-region)
         ("C-<return>" . tidal-run-line)
         ("C-S-<return>" . tidal-run-multiple-lines)
         
         ;; Control
         ("C-c C-h" . my/tidal-hush-notify)
         ("C-c C-1" . (lambda () (interactive) (my/tidal-solo 1)))
         ("C-c C-2" . (lambda () (interactive) (my/tidal-solo 2)))
         ("C-c C-0" . my/tidal-unsolo)
         
         ;; Helpers
         ("C-c C-t" . my/tidal-insert-template)))
```

---

## Keybindings Reference

| Key | Function | Description |
|-----|----------|-------------|
| `C-c C-s` | `tidal-run-line` | Evaluate current line |
| `C-c C-e` | `tidal-run-multiple-lines` | Evaluate block (between blank lines) |
| `C-c C-r` | `tidal-run-region` | Evaluate selected region |
| `C-c C-h` | `tidal-hush` | Stop all patterns |
| `C-<return>` | `tidal-run-line` | Quick evaluation |
| `C-S-<return>` | `tidal-run-multiple-lines` | Quick block evaluation |

---

## Optional: SuperCollider sclang-mode

If you want to edit SuperCollider files in Emacs (not required for Tidal):

```elisp
;;; SuperCollider Mode (Optional)
;;; Only needed if you want to edit .sc files in Emacs
;;; Most users just use the SuperCollider IDE for SC setup

(use-package sclang
  :if (file-exists-p "/usr/share/SuperCollider/Extensions/scel/el")
  :load-path "/usr/share/SuperCollider/Extensions/scel/el"
  :mode ("\\.sc\\'" . sclang-mode)
  :custom
  (sclang-help-path '("/usr/share/SuperCollider/HelpSource"))
  :config
  (defun my/start-superdirt ()
    "Start SuperDirt in SuperCollider."
    (interactive)
    (sclang-eval-string "SuperDirt.start")
    (message "SuperDirt starting...")))
```

---

## Troubleshooting

### "Could not find BootTidal.hs"

On Arch Linux, download it manually:

```bash
mkdir -p ~/.local/share/SuperCollider
wget https://raw.githubusercontent.com/tidalcycles/Tidal/master/BootTidal.hs \
  -O ~/.local/share/SuperCollider/BootTidal.hs
```

### No Sound

1. Ensure SuperCollider is running
2. In SuperCollider, evaluate: `SuperDirt.start;`
3. Check audio output in SuperCollider: `s.meter;`

### GHCi Errors

```bash
# Verify tidal is installed (Arch)
pacman -Qs haskell-tidal

# Test in ghci
ghci -e ':m Sound.Tidal.Context'
```

### "Could not find module 'Sound.Tidal.Context'"

On Arch, ensure all Haskell packages are installed:

```bash
sudo pacman -S haskell-{tidal,bifunctors,colour,hosc,mwc-random,network,primitive,random,vector,microspec}
```

---

## Quick Start Workflow

1. **Start SuperCollider** (IDE or terminal)
2. **Start SuperDirt** - evaluate in SC: `SuperDirt.start;`
3. **Open `.tidal` file** in Emacs
4. **Start Tidal** - `M-x tidal-start-haskell`
5. **Write and evaluate patterns**:

```haskell
d1 $ sound "bd sn"      -- C-<return> to evaluate
d2 $ sound "hh*8"       -- Another pattern
hush                    -- Stop everything
```

---

## Example Patterns to Try

```haskell
-- Basic beat
d1 $ sound "bd sn"

-- Layered drums
d1 $ stack [
  sound "bd*4",
  sound "~ sn",
  sound "hh*8" # gain 0.6
]

-- Euclidean rhythm
d1 $ sound "bd(3,8) sn(2,8)"

-- With effects
d1 $ sound "bd sn" # room 0.5 # delay 0.3

-- Pattern transformation
d1 $ every 4 (fast 2) $ sound "bd sn hh cp"

-- Silence
hush
```
