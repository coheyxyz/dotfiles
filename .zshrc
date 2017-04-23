autoload -U compinit
compinit

bindkey -e  # use emacs-like key bindings

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export PATH=$PATH:/usr/local/mysql/bin
export TERM=xterm-256color
export EDITOR=vim
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

setopt pushd_ignore_dups
setopt correct
setopt nolistbeep
setopt ignoreeof  # Don't logout with C-d

zstyle ':completion:*:default' menu select=2

# aliases
alias ls='ls -F'
alias g=git
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'

# command history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups
setopt inc_append_history

# command history search
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
stty stop undef  # be able to search forward with ctrl-s

# git completion
# wget -O git-completion.sh 'http://git.kernel.org/cgit/git/git.git/plain/contrib/completion/git-completion.bash?id=01b97a4cb60723d18b437efdc474503d2a9dd384'
autoload bashcompinit
bashcompinit

[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator
[[ -s ~/.cask ]] && export PATH="$HOME/.cask/bin:$PATH"

if which rbenv > /dev/null 2>&1; then eval "$(rbenv init -)"; fi
if which pyenv > /dev/null 2>&1; then eval "$(pyenv init -)"; fi
if which direnv > /dev/null 2>&1; then eval "$(direnv hook zsh)"; [ -f .envrc ] && direnv reload; fi

# load under ~/.zsh/
ZSHHOME="${HOME}/.zsh"
if [[ -d $ZSHHOME ]]; then
  for i in $ZSHHOME/*.sh; do
    if [[ $i != */env.sh ]]; then
      source $i
    fi
  done
fi

[[ -s $ZSHHOME/env.sh ]] && source $ZSHHOME/env.sh
