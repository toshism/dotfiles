set fish_function_path $fish_function_path ~/dev/projects/plugin-foreign-env/functions
set PATH /usr/local/bin $PATH
# set PATH /usr/lib/postgresql/9.4/bin $PATH
# set PATH ~/.local/bin $PATH
set PATH ~/bin $PATH
# set PATH /usr/local/go/bin $PATH
set PATH /snap/bin/ $PATH
set PATH ~/.local/bin/ $PATH
# set PATH ~/.nimble/bin $PATH
# set PATH ~/.cargo/bin $PATH
# set PATH ~/.yarn/bin $PATH
# set PATH ~/dev/projects/plan9port/bin $PATH
# set PATH ~/.roswell/bin $PATH
# eval (python -m virtualfish compat_aliases)

# set -x GOPATH /home/tosh/dev/projects/go # the -x flag exports the variable
# set PATH $PATH $GOPATH/bin
set PATH $PATH /home/tosh.lyons/go/bin
set PATH $PATH /usr/local/go/bin
set PATH $PATH /home/tosh.lyons/.cask/bin
set PATH $PATH /home/tosh.lyons/.cargo/bin # rust

#flutter
set PATH $PATH /home/tosh.lyons/snap/flutter/common/flutter/bin

#set TERM alacritty-direct
set TERM xterm-256color
set -x EDITOR "emacsclient -t"
set -x ALTERNATE_EDITOR ""
set PYTHONDONTWRITEBYTECODE true

set PKG_CONFIG_LIBDIR /usr/local/lib64/pkgconfig/ $PKG_CONFIG_LIBDIR

set KUBECONFIG /home/tosh.lyons/kubeconfig
# set KUBECONFIG /etc/rancher/k3s/k3s.yaml
set GUIX_PROFILE "/home/tosh.lyons/.guix-profile"
fenv source "/home/tosh.lyons/.guix-profile/etc/profile"


# alias mux="tmuxinator"
# alias cdrar="cd ~/dev/projects/uniregistrar"
alias cdp="cd ~/dev/projects"
alias cdg="cd /home/tosh.lyons/go/src"
# alias cds="cd $GOPATH/src/bitbucket.uniregistry.com/rar/swerve"
alias cds="cd ~/dev/projects/scoutbee"
#alias kubectl="microk8s.kubectl"
alias cdp='cd (ls -d ~/dev/projects/* | gum filter --placeholder "Select project:")'
alias cds='cd (ls -d ~/dev/projects/scoutbee/* | gum filter)'


function fish_prompt
	 test $SSH_TTY; and printf (set_color red)(whoami)(set_color white)'@'(set_color yellow)(hostname)' '

	 test $USER = 'root'; and echo (set_color red)"#"

	 # Main
	 echo -n (set_color cyan)(prompt_pwd) (virtual_env_prompt) (set_color 3a4c55)'>'(set_color 4a626d)'>'(set_color 5b7786)'> '
end

function virtual_env_prompt
	 if set -q VIRTUAL_ENV
	    echo -n -s (set_color blue) "(" (basename "$VIRTUAL_ENV") ")" (set_color normal) " "
	 end
end

function fish_right_prompt
	 # last status
	 test $status != 0; and printf (set_color red)"⏎ "

	 #if git rev-parse ^ /dev/null
	 if git rev-parse --show-toplevel 2> /dev/null
	    # Purple if branch detached else green
	    git branch -qv | grep "\*" | grep -q detached
	    	and set_color purple --bold
		or set_color green --bold

# Need optimization on this block (eliminate space)
git name-rev --name-only HEAD

# Merging state
# git merge -q ^ /dev/null; or printf ':'(set_color red)'merge'
# printf ' '

# Symbols
for i in (git branch -qv --no-color|grep \*|cut -d' ' -f4-|cut -d] -f1|tr , \n)\
(git status --porcelain | cut -c 1-2 | uniq)
switch $i
case "*[ahead *"
  printf (set_color purple)⬆' '
case "*behind *"
  printf (set_color purple)⬇' '
case "."
  printf (set_color green)✚' '
case " D"
  printf (set_color red)✖' '
case "*M*"
  printf (set_color blue)✱' '
case "*R*"
  printf (set_color purple)➜' '
case "*U*"
  printf (set_color brown)═' '
case "??"
  printf (set_color white)◼' '
end
end
end
end

# pyenv stuff
set -x PATH "/home/tosh.lyons/.pyenv/bin" $PATH
status --is-interactive; and . (pyenv init -|psub)
status --is-interactive; and . (pyenv virtualenv-init -|psub)

# perl/plenv
# set -x PATH "/home/tosh/.plenv/bin" $PATH
# status --is-interactive; and . (plenv init -|psub)

# nodenv stuff
# set -x PATH "/home/tosh/.nodenv/bin" $PATH
# status --is-interactive; and . (nodenv init -|psub)

# mount fake ext4 dir for dropbox
# if status --is-login
#     systemctl --user restart dropbox-zfs.service
# end
#set fish_function_path $fish_function_path ~/dev/projects/plugin-foreign-env/functions

eval (direnv hook fish)

function fish_greeting
	 #fortune -s
	 ~/bin/fortunecookie
end

function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
functions --copy fish_prompt vterm_old_fish_prompt
function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end