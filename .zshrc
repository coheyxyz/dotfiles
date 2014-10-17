autoload -U compinit
compinit

bindkey -e  # use emacs-like key bindings

export PATH=$PATH:/usr/local/mysql/bin
export TERM=xterm-256color
export EDITOR=vim

setopt pushd_ignore_dups
setopt correct
setopt nolistbeep
setopt ignoreeof  # Don't logout with C-d

# env
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# aliases
alias ls='ls -F'
alias r=rails
# aliases for git
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

# tmuxinator
[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator

# rbenv
if which rbenv > /dev/null 2>&1; then eval "$(rbenv init -)"; fi

# cask
[[ -s ~/.cask ]] && export PATH="$HOME/.cask/bin:$PATH"

# load under ~/.zsh/
ZSHHOME="${HOME}/.zsh"
if [ -d $ZSHHOME -a -r $ZSHHOME -a -x $ZSHHOME ]; then
  for i in $ZSHHOME/*; do
    [[ ${i##*/} = *.sh ]] &&
    [ \( -f $i -o -h $i \) -a -r $i ] && . $i
  done
fi

[[ -s $HOME/.zsh/env.sh ]] && source $HOME/.zsh/env.sh
