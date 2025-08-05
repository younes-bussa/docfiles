#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ip='ip -color=auto'

alias video='firejail --net=none vlc'
alias pdf='firejail --net=none evince'

# Function to get plain git branch
parse_git_branch() {
  git rev-parse --is-inside-work-tree &>/dev/null || return
  git symbolic-ref --short HEAD 2>/dev/null
}

# Colorful prompt with correct coloring of git branch
export PS1='\[\e[1;32m\]\u@\h \[\e[1;34m\]\w\[\e[0;33m\]$(b=$(parse_git_branch); [ -n "$b" ] && echo " (\[\e[1;35m\]$b\[\e[0;33m\])")\[\e[0m\] \$ '



if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
fi


HISTSIZE=5000
HISTFILESIZE=10000
HISTCONTROL=ignoredups:erasedups
HISTTIMEFORMAT='%F %T '

# Save each command immediately
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"



export COLORTERM=truecolor

