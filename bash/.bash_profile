export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export NVM_DIR=~/.nvm
. $(brew --prefix nvm)/nvm.sh

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/local/lib/python2.7/site-packages/powerline/bindings/bash/powerline.sh

# Set architecture flags
export ARCHFLAGS="-arch x86_64"
# Ensure user-installed binaries take precedence
export PATH=/usr/local/bin:$PATH
# Load .bashrc if it exists
# test -f ~/.bashrc && source ~/.bashrc


if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

export GOPATH=$HOME/dev/go
export PATH=$PATH:$GOPATH/bin:/usr/local/Cellar/postgresql/9.4.5/bin
# add gnu utils path
PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"
# and their man path
MANPATH="/usr/local/opt/gnu-tar/libexec/gnuman:$MANPATH"

source /usr/local/bin/virtualenvwrapper.sh

# GO

alias ll='ls -l'
alias la='ls -a'
alias l='ls -la'
# alias es='/usr/local/Cellar/emacs-mac/emacs-24.5-rc1-mac-5.6/bin/emacs -nw'
# alias ec='/usr/local/Cellar/emacs-mac/emacs-24.5-rc1-mac-5.6/bin/emacsclient -t'
alias ec='/usr/local/Cellar/emacs-mac/HEAD/bin/emacsclient -t --alternate-editor="" -c "$@"'
alias eg='/usr/local/Cellar/emacs-mac/HEAD/bin/emacsclient --alternate-editor="" -c "$@"'

# export EDITOR='/usr/local/Cellar/emacs-mac/HEAD/bin/emacsclient -t --alternate-editor="" -c "$@"'
# exec emacsclient --alternate-editor="" -c "$@"
export EDITOR=vim

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

alias weekly_report="git log --pretty=format:'%ad: %s (%h)' --date=short --reverse --all --since=8.days.ago --author='tosh'"

alias reset_test_db='cd /Users/tosh/dev/projects/registry/uniregistrar/schema; make DBNAME=uniregistrar_test dropdb; make DBNAME=uniregistrar_test all; cd -'
export PATH="/usr/local/sbin:$PATH"

epoch_date() {
  echo $(date +%Y%m%d%H%M)
}
#alias _now='date "+%Y%m%d%H%M"'
#DEPLOYDIR="~/dev/projects/registry/rar-deploy/uniregistrar"
alias git-diff="git log --graph master..dev"
#alias cddeploy="cd $DEPLOYDIR"
#alias deploy-update="git checkout dev && git pull && git checkout master && git pull"
#alias ymd='echo _now'
#alias save-diff="echo $_now" # git-diff > ~/dev/projects/registry/rar-deploy/diffs/$(epoch_date).txt"
#alias merge-deploy="git checkout master && git merge dev"
#alias prepare-deploy="cddeploy && deploy-update && save-diff && merge-deploy"
alias prepare-deploy=~/dev/projects/registry/rar-deploy/prepare-deploy.sh

export HISTFILESIZE=
export HISTSIZE=
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
shopt -s histappend # append to history, don't overwrite it
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
