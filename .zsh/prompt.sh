autoload -U colors; colors

setopt prompt_subst
setopt transient_rprompt

PROMPT="%{$fg_bold[green]%}%n@%m%# $DEFAULT%{$reset_color%}%b"
RPROMPT="[%~]"
SPROMPT="correct: %R -> %r ? "

# git
# http://d.hatena.ne.jp/uasi/20091017/1255712789
# ${fg[...]} や $reset_color をロード
autoload -U colors; colors

function rprompt-git-current-branch {
        local name st color

        if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
                return
        fi
        name=$(basename "`git symbolic-ref HEAD 2> /dev/null`")
        if [[ -z $name ]]; then
                return
        fi
        st=`git status 2> /dev/null`
        if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
                color=${fg[green]}
        elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
                color=${fg[yellow]}
        elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
                color=${fg_bold[red]}
        else
                color=${fg[red]}
        fi

        # %{...%} は囲まれた文字列がエスケープシーケンスであることを明示する
        # これをしないと右プロンプトの位置がずれる
        echo "%{$color%}$name%{$reset_color%} "
}

setopt prompt_subst
setopt transient_rprompt

RPROMPT='[`rprompt-git-current-branch`%~]'
