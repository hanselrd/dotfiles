# the idea of this theme is to contain a lot of info in a small string, by
# compressing some parts and colorcoding, which bring useful visual cues,
# while limiting the amount of colors and such to keep it easy on the eyes.
# When a command exited >0, the timestamp will be in red and the exit code
# will be on the right edge.
# The exit code visual cues will only display once.
# (i.e. they will be reset, even if you hit enter a few times on empty command prompts)

typeset -A host_repr

# translate hostnames into shortened, colorcoded strings
host_repr=('dieter-ws-a7n8x-arch' "%{$fg_bold[green]%}ws" 'dieter-p4sci-arch' "%{$fg_bold[blue]%}p4")

# local time, color coded by last return code
time_enabled="%(?.%{$fg[green]%}.%{$fg[red]%})%*%{$reset_color%}"
time_disabled="%{$fg[green]%}%*%{$reset_color%}"
time=$time_enabled

# user part, color coded by privileges
if [[ $USER == "root" ]]; then
    local user="%(!.%{$fg[red]%}.%{$fg[red]%})%n%{$reset_color%}"
else
    local user="%(!.%{$fg[blue]%}.%{$fg[blue]%})%n%{$reset_color%}"
fi

# Hostname part.  compressed and colorcoded per host_repr array
# if not found, regular hostname in default color
local host="@${host_repr[$HOST]:-$HOST}%{$reset_color%}"

# Compacted $PWD
local pwd="%{$fg[blue]%}%c%{$reset_color%}"

function locked_idle_prompt_info() {
    ls /var/lock/prevent_idle_terminate &>/dev/null
    rc=$?
    if [[ "${rc}" == 0 ]]; then
        echo "%{$fg_bold[yellow]%}LOCKED%{$reset_color%} "
    else
        ls /etc/idle_terminate_threshold &>/dev/null
        rc=$?
        if [[ "${rc}" == 0 ]]; then
            out=$(cat /etc/idle_terminate_threshold)
            echo "%{$fg_bold[yellow]%}IDLE%{$reset_color%} %{$fg[yellow]%}$out%{$reset_color%} "
        fi
    fi
}

PROMPT='$(locked_idle_prompt_info)${time} ${user}${host%%.*} ${pwd} $(git_prompt_info)'

MODE_INDICATOR="%{$fg_bold[magenta]%}❮%{$reset_color%}%{$fg[magenta]%}❮❮%{$reset_color%}"

function vi_mode_prompt_info() {
    echo "${${VI_KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
}

# i would prefer 1 icon that shows the "most drastic" deviation from HEAD,
# but lets see how this works out
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%} %{$fg[yellow]%}?%{$fg[green]%}%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}"

# elaborate exitcode on the right when >0
return_code_enabled="%(?..%{$fg[red]%}%? ⬣%{$reset_color%})"
return_code_disabled=
return_code=$return_code_enabled

RPS1='$(vi_mode_prompt_info) ${return_code}'

function accept-line-or-clear-warning () {
    if [[ -z $BUFFER ]]; then
        time=$time_disabled
        return_code=$return_code_disabled
    else
        time=$time_enabled
        return_code=$return_code_enabled
    fi
    zle accept-line
}
zle -N accept-line-or-clear-warning
bindkey '^M' accept-line-or-clear-warning
