let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

in  ''
    set -g @plugin 'tmux-plugins/tpm'
    set -g @plugin 'tmux-plugins/tmux-sensible'

    set -g default-terminal "screen-256color"
    set -g display-panes-time 5000
    set -g history-limit 50000
    setw -g mode-keys vi
    set -g prefix C-a
    unbind C-b
    bind C-a send-prefix
    if-shell "test -f ${Directory/toText
                          Directory.Tmux2}/line.conf" "source ${Directory/toText
                                                                  Directory.Tmux2}/line.conf"

    if "test ! -d ${Directory/toText Directory.Tmux2}/plugins/tpm" \
        "run 'git clone https://github.com/tmux-plugins/tpm ${Directory/toText
                                                                Directory.Tmux2}/plugins/tpm && ${Directory/toText
                                                                                                    Directory.Tmux2}/plugins/tpm/bin/install_plugins'"
    run -b '${Directory/toText Directory.Tmux2}/plugins/tpm/tpm'
    ''
