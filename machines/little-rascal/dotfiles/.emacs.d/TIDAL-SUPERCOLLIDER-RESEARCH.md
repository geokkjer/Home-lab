# Tidal Cycles & SuperCollider Integration in Emacs

## Overview

Tidal Cycles (or just "Tidal") is a live coding environment for algorithmic patterns, used for musical improvisation and composition. It uses Haskell as its language and SuperCollider as its audio engine. Emacs is one of the primary editors for Tidal live coding.

## Architecture

```
┌─────────────┐     ┌─────────────┐     ┌──────────────┐
│   Emacs     │────▶│   Tidal     │────▶│ SuperCollider│────▶ Audio
│ (tidal-mode)│ OSC │  (ghci)     │ OSC │  (SuperDirt) │
└─────────────┘     └─────────────┘     └──────────────┘
```

1. **Emacs** - Editor with `tidal.el` mode for writing patterns
2. **Tidal** - Haskell library that interprets patterns, sends OSC messages
3. **SuperCollider** - Audio synthesis engine
4. **SuperDirt** - Tidal's sample playback and synthesis system in SC

## Components Needed

### 1. SuperCollider

- Audio synthesis environment
- Runs the `scsynth` audio server
- Hosts SuperDirt quark for sample playback

### 2. SuperDirt

- SuperCollider extension (quark)
- Handles sample playback, effects, synthesis
- Receives OSC from Tidal

### 3. Tidal Cycles

- Haskell library
- Pattern language for live coding
- Communicates with SuperDirt via OSC

### 4. Emacs Integration

- `tidal.el` - Major mode for Tidal
- `sclang-mode` - SuperCollider language mode (optional)

## Installation Options

### Option A: Arch Linux (Native)

```bash
# SuperCollider
sudo pacman -S supercollider sc3-plugins

# Haskell tooling
sudo pacman -S ghc cabal-install

# Tidal Cycles
cabal update
cabal install tidal

# SuperDirt (in SuperCollider)
# Run: Quarks.install("SuperDirt")
```

### Option B: Via Nix

```nix
# In your NixOS config or home-manager
environment.systemPackages = with pkgs; [
  supercollider
  sc3-plugins
  haskellPackages.tidal
  ghc
];
```

### Option C: Using the Tidal installer

```bash
# Automated installer (cross-platform)
curl https://raw.githubusercontent.com/tidalcycles/tidal-bootstrap/main/tidal-bootstrap.sh | bash
```

## Emacs Configuration

### Basic tidal.el Setup

```elisp
(use-package tidal
  :ensure t
  ;; Path to tidal in your system (adjust for your setup)
  :custom
  (tidal-interpreter "ghci")
  (tidal-boot-script-path "~/.cabal/share/tidal-1.9.5/BootTidal.hs")
  :config
  ;; Key bindings for live coding
  (define-key tidal-mode-map (kbd "C-c C-s") 'tidal-run-line)
  (define-key tidal-mode-map (kbd "C-c C-e") 'tidal-run-multiple-lines)
  (define-key tidal-mode-map (kbd "C-c C-r") 'tidal-run-region)
  (define-key tidal-mode-map (kbd "C-c C-h") 'tidal-hush))
```

### Finding BootTidal.hs Path

```bash
# Find the boot script location
find ~/.cabal -name "BootTidal.hs" 2>/dev/null
# or for stack
find ~/.stack -name "BootTidal.hs" 2>/dev/null
# or for nix
find /nix/store -name "BootTidal.hs" 2>/dev/null | head -1
```

### Alternative: quelpa/straight installation

```elisp
;; If tidal.el isn't in MELPA
(use-package tidal
  :quelpa (tidal :fetcher github :repo "tidalcycles/Tidal" :files ("tidal.el"))
  :custom
  (tidal-interpreter "ghci")
  (tidal-boot-script-path "/path/to/BootTidal.hs"))
```

## SuperCollider Emacs Integration (Optional)

For editing SuperCollider code directly in Emacs:

```elisp
(use-package sclang
  :ensure nil ; Usually comes with SC installation
  :load-path "/usr/share/SuperCollider/Extensions/scel/el"
  :mode ("\\.sc\\'" . sclang-mode)
  :config
  (setq sclang-help-path '("/usr/share/SuperCollider/HelpSource")))
```

**Note**: `scel` (SuperCollider Emacs Lisp) is an optional component that provides `sclang-mode`. Many Tidal users just use the SuperCollider IDE for SC setup and Emacs only for Tidal.

## Workflow

### 1. Start SuperCollider + SuperDirt

In SuperCollider IDE or via sclang:

```supercollider
// Start SuperDirt
SuperDirt.start;

// Or with more outputs
(
s.options.numBuffers = 1024 * 256;
s.options.memSize = 8192 * 32;
s.options.numWireBufs = 64;
s.options.maxNodes = 1024 * 32;
SuperDirt.start;
)
```

### 2. Open Tidal in Emacs

```elisp
;; Open a .tidal file, then:
M-x tidal-start-haskell
;; Or with keybinding:
C-c C-s (after configuring)
```

### 3. Live Code

```haskell
-- In your .tidal file
d1 $ sound "bd sn"

d1 $ sound "bd*4 sn:2"

d1 $ slow 2 $ sound "bd sn hh cp"

d1 $ every 4 (fast 2) $ sound "bd sn hh*2 cp"

-- Silence
hush
```

## Key Bindings Reference

| Key | Function | Description |
|-----|----------|-------------|
| `C-c C-s` | `tidal-run-line` | Evaluate current line |
| `C-c C-e` | `tidal-run-multiple-lines` | Evaluate block (between blank lines) |
| `C-c C-r` | `tidal-run-region` | Evaluate selected region |
| `C-c C-h` | `tidal-hush` | Stop all patterns |
| `C-c C-c` | `tidal-run-block` | Run current block |
| `C-c C-q` | `tidal-quit-haskell` | Quit Tidal |

## Sample Configuration for Literate Config

### Minimal

```elisp
(use-package tidal
  :mode ("\\.tidal\\'" . tidal-mode)
  :custom
  (tidal-interpreter "ghci")
  ;; Update this path!
  (tidal-boot-script-path "~/.cabal/share/tidal-1.9.5/BootTidal.hs"))
```

### Standard (Recommended)

```elisp
(use-package tidal
  :mode ("\\.tidal\\'" . tidal-mode)
  :custom
  (tidal-interpreter "ghci")
  (tidal-boot-script-path 
    (or (car (file-expand-wildcards "~/.cabal/share/tidal-*/BootTidal.hs"))
        "/nix/store/*-tidal-*/share/BootTidal.hs"))
  :config
  ;; Custom hush function that's more visible
  (defun my/tidal-hush-confirm ()
    "Hush with confirmation message."
    (interactive)
    (tidal-hush)
    (message "All patterns silenced!"))
  
  :bind (:map tidal-mode-map
         ("C-c C-h" . my/tidal-hush-confirm)
         ("C-<return>" . tidal-run-line)))
```

### Full (with SuperCollider)

```elisp
;; Tidal Cycles
(use-package tidal
  :mode ("\\.tidal\\'" . tidal-mode)
  :custom
  (tidal-interpreter "ghci")
  (tidal-boot-script-path "~/.cabal/share/tidal-1.9.5/BootTidal.hs")
  :config
  (defun my/tidal-start-all ()
    "Start SuperCollider and Tidal."
    (interactive)
    ;; Could add SC startup here if using sclang
    (tidal-start-haskell)
    (message "Tidal started! Remember to start SuperDirt in SC.")))

;; SuperCollider (optional - if scel is installed)
(use-package sclang
  :if (file-exists-p "/usr/share/SuperCollider/Extensions/scel/el")
  :load-path "/usr/share/SuperCollider/Extensions/scel/el"
  :mode ("\\.sc\\'" . sclang-mode)
  :config
  (defun my/start-superdirt ()
    "Start SuperDirt in SuperCollider."
    (interactive)
    (sclang-eval-string "SuperDirt.start")))
```

## Arch Linux Full Setup Guide

```bash
# 1. Install SuperCollider
sudo pacman -S supercollider sc3-plugins jack2

# 2. Install Haskell
sudo pacman -S ghc cabal-install

# 3. Update cabal and install Tidal
cabal update
cabal install tidal --lib

# 4. Start JACK (for audio routing)
jack_control start

# 5. Open SuperCollider IDE and install SuperDirt
# In SC IDE, run:
#   Quarks.install("SuperDirt");
#   Quarks.install("Dirt-Samples");
# Then recompile: Ctrl+Shift+L

# 6. In SC IDE, start SuperDirt:
#   SuperDirt.start;

# 7. Find your BootTidal.hs path
find ~/.cabal -name "BootTidal.hs"

# 8. Update your Emacs config with that path

# 9. Open a .tidal file in Emacs and start coding!
```

## Troubleshooting

### Common Issues

1. **"Could not find BootTidal.hs"**
   - Run `find ~ -name "BootTidal.hs"` to locate it
   - Update `tidal-boot-script-path` in config

2. **No sound**
   - Check SuperDirt is running in SuperCollider
   - Check audio routing (JACK/PulseAudio)
   - Try `s.meter` in SC to see if signals arrive

3. **"Cannot connect to scsynth"**
   - SuperCollider server not running
   - Start with `Server.default.boot` in SC

4. **GHCi errors**
   - Ensure tidal is installed: `cabal list --installed | grep tidal`
   - Check GHC version compatibility

### Checking Versions

```bash
# SuperCollider
scsynth -v

# GHC
ghc --version

# Tidal (in ghci)
ghci -e ':m Sound.Tidal.Context' -e 'tidal_version'
```

## Resources

- **Tidal Cycles**: <https://tidalcycles.org>
- **Tidal Documentation**: <https://tidalcycles.org/docs>
- **SuperCollider**: <https://supercollider.github.io>
- **SuperDirt**: <https://github.com/musikinformatik/SuperDirt>
- **tidal.el**: <https://github.com/tidalcycles/Tidal/blob/main/tidal.el>
- **Tidal Club (community)**: <https://club.tidalcycles.org>

## Learning Resources

- **Tidal Tutorial**: <https://tidalcycles.org/docs/getting-started/tutorial>
- **Pattern Reference**: <https://tidalcycles.org/docs/reference/mini_notation>
- **Alex McLean's Tutorials** (Tidal creator): YouTube
- **Toplap** (live coding community): <https://toplap.org>

## Example Patterns

```haskell
-- Basic beat
d1 $ sound "bd sn"

-- With hi-hats
d1 $ sound "bd sn" # speed 1
d2 $ sound "hh*8"

-- Euclidean rhythms
d1 $ sound "bd(3,8)"

-- Layering
d1 $ stack [
  sound "bd*4",
  sound "~ sn",
  sound "hh*8"
]

-- Effects
d1 $ sound "bd sn" # crush 4 # room 0.5

-- Pattern transformation
d1 $ every 4 (fast 2) $ sound "bd sn hh cp"

-- Silence everything
hush
```

## Recommendation for Literate Config

For the `Emacs.org` config, I recommend the **Minimal** or **Standard** configuration:

1. Keep it simple - just `tidal.el` setup
2. Document the external dependencies (SC, cabal install)
3. Let users configure the `BootTidal.hs` path themselves
4. Skip `sclang-mode` unless specifically needed

The external setup (SuperCollider + SuperDirt + cabal install tidal) should be documented but not automated, as audio software setup is system-specific.

## Running Tidal in Org-Mode (ob-tidal)

Yes! You can run Tidal code blocks directly in org-mode using `ob-tidal`, which provides org-babel integration for Tidal Cycles.

### How It Works

```
┌─────────────────┐     ┌─────────────┐     ┌──────────────┐
│   Org-Mode      │────▶│   Tidal     │────▶│ SuperCollider│
│ (ob-tidal)      │     │  (ghci)     │     │  (SuperDirt) │
│                 │     │             │     │              │
│ #+begin_src     │     │ persistent  │     │              │
│  tidal          │     │ REPL        │     │              │
│ #+end_src       │     │             │     │              │
└─────────────────┘     └─────────────┘     └──────────────┘
```

The key difference: `ob-tidal` uses a **persistent session** - the ghci REPL stays running, so patterns continue playing after you evaluate them (just like in a `.tidal` file).

### Installing ob-tidal

`ob-tidal` is not in MELPA, so install via quelpa or straight:

```elisp
;; Via quelpa
(use-package ob-tidal
  :quelpa (ob-tidal :fetcher github :repo "tidalcycles/ob-tidal")
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(tidal . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; Or via straight.el
(use-package ob-tidal
  :straight (ob-tidal :type git :host github :repo "tidalcycles/ob-tidal")
  :after org)
```

### Configuration

```elisp
;; Full ob-tidal setup
(use-package ob-tidal
  :quelpa (ob-tidal :fetcher github :repo "tidalcycles/ob-tidal")
  :after (org tidal)
  :custom
  ;; Use the same interpreter as tidal-mode
  (ob-tidal-interpreter "ghci")
  ;; Same boot script path
  (ob-tidal-boot-script-path "~/.local/share/SuperCollider/BootTidal.hs")
  :config
  ;; Register tidal with org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((tidal . t)))))
```

### Usage in Org-Mode

Once configured, you can write Tidal patterns in org source blocks:

```org
#+begin_src tidal
d1 $ sound "bd sn"
#+end_src
```

Evaluate with `C-c C-c` on the block. The pattern starts playing!

#### Session-Based Workflow

All blocks share the same Tidal session:

```org
* My Live Coding Session

Start a simple beat:
#+begin_src tidal
d1 $ sound "bd sn"
#+end_src

Add hi-hats:
#+begin_src tidal
d2 $ sound "hh*8" # gain 0.7
#+end_src

Transform the beat:
#+begin_src tidal
d1 $ every 4 (fast 2) $ sound "bd sn hh cp"
#+end_src

Silence everything:
#+begin_src tidal
hush
#+end_src
```

### Key Bindings in Org-Mode

| Key | Action |
|-----|--------|
| `C-c C-c` | Evaluate current block (sends to Tidal REPL) |
| `C-c '` | Edit block in dedicated buffer |
| `C-c C-v C-b` | Execute all blocks in buffer |

### Advantages of Org-Mode + Tidal

1. **Literate live coding** - Mix documentation with patterns
2. **Persistent session** - REPL stays alive between blocks
3. **Organized sets** - Structure performances with headings
4. **Export** - Export to HTML/PDF for sharing/teaching
5. **Version control** - Document your creative process
6. **Notes alongside code** - Explain what each pattern does

### Example: Live Coding Document

```org
#+TITLE: Algorave Set - November 2025
#+PROPERTY: header-args:tidal :session *tidal*

* Intro (4 bars)

Simple kick and snare to start:

#+begin_src tidal
d1 $ sound "bd sn"
#+end_src

* Build-up

Add euclidean hi-hats:

#+begin_src tidal
d2 $ sound "hh(5,8)" # gain 0.6
#+end_src

* Drop

Full pattern with effects:

#+begin_src tidal
d1 $ stack [
  sound "bd*4",
  sound "~ sn" # room 0.3,
  sound "hh(5,8)" # gain 0.6
] # delay 0.2

d3 $ sound "bass:3" # speed 0.5
#+end_src

* Breakdown

Strip back to minimal:

#+begin_src tidal
d1 $ slow 2 $ sound "bd ~ ~ sn"
d2 $ silence
d3 $ silence
#+end_src

* End

#+begin_src tidal
hush
#+end_src
```

### Alternative: Using Haskell Blocks

If `ob-tidal` doesn't work, you can use `ob-haskell` with a custom session:

```elisp
;; Fallback: use haskell blocks with tidal loaded
(setq org-babel-default-header-args:haskell
      '((:session . "*tidal*")
        (:results . "silent")))
```

Then manually start Tidal first (`M-x tidal-start-haskell`) and use:

```org
#+begin_src haskell :session *tidal*
d1 $ sound "bd sn"
#+end_src
```

This is less elegant but works without ob-tidal.

### Troubleshooting ob-tidal

1. **"No org-babel-execute function for tidal"**
   - Ensure ob-tidal is loaded: `(require 'ob-tidal)`
   - Check `org-babel-load-languages` includes `(tidal . t)`

2. **Session not starting**
   - Verify `ob-tidal-boot-script-path` is correct
   - Start manually first: `M-x tidal-start-haskell`

3. **Patterns not playing**
   - Ensure SuperDirt is running in SuperCollider
   - Check the `*tidal*` buffer for errors

### Recommendation

For a literate live coding setup, add this to your `Emacs.org`:

```elisp
;; Tidal Cycles in Org-Mode
(use-package ob-tidal
  :quelpa (ob-tidal :fetcher github :repo "tidalcycles/ob-tidal")
  :after (org tidal)
  :custom
  (ob-tidal-boot-script-path
   (or (and (file-exists-p "~/.local/share/SuperCollider/BootTidal.hs")
            (expand-file-name "~/.local/share/SuperCollider/BootTidal.hs"))
       (car (file-expand-wildcards "~/.cabal/share/tidal-*/BootTidal.hs"))))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((tidal . t)))))
```

This enables a beautiful workflow: write patterns in org-mode, evaluate with `C-c C-c`, document your creative process, and export for sharing!
