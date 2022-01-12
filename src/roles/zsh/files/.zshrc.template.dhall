let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    # Setup Antigen
    if [[ ! -f "${env.user_home_dir}/.antigen/antigen.zsh" ]]; then
        curl -fLo "${env.user_home_dir}/.antigen/antigen.zsh" --create-dirs \
            https://git.io/antigen
    fi
    source "${env.user_home_dir}/.antigen/antigen.zsh"

    antigen use oh-my-zsh

    antigen bundle command-not-found
    antigen bundle pip
    antigen bundle vi-mode

    export NVM_LAZY_LOAD=true
    export NVM_LAZY_LOAD_EXTRA_COMMANDS=("vim")
    antigen bundle lukechilds/zsh-nvm

    antigen bundle zdharma-continuum/fast-syntax-highlighting

    antigen bundle zsh-users/zsh-autosuggestions
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=0,bg=15,underline,standout"

    antigen bundle zsh-users/zsh-completions

    antigen theme denysdovhan/spaceship-prompt
    SPACESHIP_PROMPT_ORDER=(
    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Remote
          then  Some
                  ''
                      custom
                  ''
          else  None Text
        )}
        time
        user
        dir
        host
        git
        venv
        exec_time
        line_sep
        vi_mode
        jobs
        exit_code
        char
    )
    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Remote
          then  Some
                  ''
                  spaceship_custom() {
                      [[ -f /var/lock/prevent_idle_terminate || -f /etc/idle_terminate_threshold ]] || return

                      local 'custom_status'

                      if [[ -f /var/lock/prevent_idle_terminate ]]; then
                          custom_status="LOCKED"
                      elif [[ -f /etc/idle_terminate_threshold ]]; then
                          custom_status="IDLE $(cat /etc/idle_terminate_threshold)"
                      fi

                      [[ -z "$custom_status" ]] && return

                      spaceship::section \
                          "red" \
                          "$SPACESHIP_PROMPT_DEFAULT_PREFIX" \
                          " $custom_status" \
                          "$SPACESHIP_PROMPT_DEFAULT_SUFFIX"
                  }
                  ''
          else  None Text
        )}
    SPACESHIP_CHAR_SYMBOL="❱ "
    SPACESHIP_TIME_SHOW=true
    SPACESHIP_TIME_FORMAT="%D{%Y-%m-%dT%T%z}"
    # SPACESHIP_TIME_12HR=true
    SPACESHIP_EXIT_CODE_SHOW=true

    antigen apply

    eval spaceship_vi_mode_enable

    # Set PATH
    ${if    Configuration/equal env.configuration Configuration.Remote
      then  ''
            export PATH="${env.user_home_dir}/.cargo/bin:${env.user_home_dir}/.local/bin:${env.user_home_dir}/sources/opt/llvm7/bin:$PATH"
            ''
      else  ''
            export PATH="/usr/lib/ccache/bin:/opt/llvm/bin:${env.user_home_dir}/.cargo/bin:${env.user_home_dir}/.local/bin:$PATH"
            ''}

    # Load ghcup
    if [[ -f "${env.user_home_dir}/.ghcup/env" ]]; then
        source "${env.user_home_dir}/.ghcup/env"
    fi

    # Load wal
    #if [[ -d "${env.user_cache_dir}/wal" ]]; then
    #    (cat "${env.user_cache_dir}/wal/sequences" &)
    #    source "${env.user_cache_dir}/wal/colors-tty.sh"
    #fi

    # User configuration
    setopt nonomatch
    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Remote
          then  Some
                  ''
                  source "${env.user_home_dir}/.bashrc" > /dev/null 2>&1
                  ''
          else  None Text
        )}

    rcp() {
        rsync -avzP $1 $2
    }

    rmv() {
        rsync -avzP --remove-source-files $1 $2
    }

    alias vi="vim -u NONE -U NONE -N -i NONE"
    alias dryclean="find . \( -name 'node_modules' -o -name 'elm-stuff' -o -name 'target' -o -name 'build' -o -name 'dist' -o -name 'venv' \) -type d -print -prune"
    alias clean="find . \( -name 'node_modules' -o -name 'elm-stuff' -o -name 'target' -o -name 'build' -o -name 'dist' -o -name 'venv' \) -type d -print -prune -exec rm -rfI {} \;"
    alias ydlaudio="youtube-dl -f bestaudio --extract-audio --audio-format mp3 --audio-quality 320k --embed-thumbnail --add-metadata --no-post-overwrites --geo-bypass --ignore-errors --restrict-filenames --output-na-placeholder=''' -o '%(title)s-%(id)s.%(ext)s'"
    alias ?="lynx https://google.com"

    [ -f ${env.user_home_dir}/.fzf.zsh ] && source ${env.user_home_dir}/.fzf.zsh
    ''
