export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Set architecture flags
export ARCHFLAGS="-arch x86_64"
# Ensure user-installed binaries take precedence
export PATH=/usr/local/bin:$PATH
# Load .bashrc if it exists
# test -f ~/.bashrc && source ~/.bashrc


export GOPATH=$HOME/dev/projects/go

# source /usr/local/bin/virtualenvwrapper.sh

alias ll='ls -l'
alias la='ls -a'
alias l='ls -la'

# exec emacsclient --alternate-editor="" -c "$@"
#export EDITOR=emacsclient --alternate-editor="" -c "$@"
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

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
if [ -e /home/tosh/.nix-profile/etc/profile.d/nix.sh ]; then . /home/tosh/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# mount fake ext4 drive for dropbox
systemctl --user restart dropbox-zfs.service
