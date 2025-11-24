# ~/.zshrc - Shell configuration for little-rascal

# Completion configuration
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' matcher-list ''
zstyle ':completion:*' menu select
zstyle ':completion:*:descriptions' format '%d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle :compinstall filename '/home/geir/.zshrc'

# Initialize completion system
autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qNmh+24) ]]; then
  compinit
else
  compinit -C
fi

# History settings
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Shell options
setopt autocd extendedglob nomatch
unsetopt beep

# Emacs-style keybindings
bindkey -e

# Enhanced word navigation
bindkey '^[f' forward-word
bindkey '^[b' backward-word
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word

# Path
export PATH="$HOME/.local/bin:$PATH"

# Starship prompt
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init zsh)"
fi

# Directory navigation with direnv
if command -v direnv >/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# Smart cd with zoxide
if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi

# Fun startup (optional - requires fortune, cowsay, and lolcat)
if command -v fortune >/dev/null 2>&1 && command -v cowsay >/dev/null 2>&1; then
  fortune -s | cowsay -f dragon | lolcat 2>/dev/null || fortune -s | cowsay -f dragon || true
fi