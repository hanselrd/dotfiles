let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Font = ../../../Lib/Font/Enum.partial.dhall

let Font/equal = ../../../codegen/Lib/Font/equal.partial.dhall

let Font/toMetadata = ../../../Lib/Font/toMetadata.partial.dhall

let env = ../../../codegen/environment.partial.dhall

let fontMetadata = Font/toMetadata env.font

in  ''
    {
        "workbench.colorTheme": "One Dark Pro",
        "workbench.iconTheme": "material-icon-theme",
        "telemetry.enableCrashReporter": false,
        "telemetry.enableTelemetry": false,
        "editor.bracketPairColorization.enabled": true,
        "editor.guides.bracketPairs": "active",
        "editor.fontFamily": "${fontMetadata.editor.name}, 'Droid Sans Mono', 'monospace', monospace, 'Droid Sans Fallback'",
        "editor.fontLigatures": ${if    Font/equal env.font Font.JetBrainsMono
                                  then  "true"
                                  else  "false"},
        "editor.fontWeight": "${External/Prelude.Text.default
                                  fontMetadata.editor.style}",
        "editor.fontSize": ${External/Prelude.Natural.show
                               fontMetadata.editor.size},
        "terminal.integrated.fontFamily": "${fontMetadata.editor.name}",
        "terminal.integrated.fontWeight": "${External/Prelude.Text.default
                                               fontMetadata.editor.style}",
        "terminal.integrated.fontSize": ${External/Prelude.Natural.show
                                            fontMetadata.editor.size},
        "tws.highlightTrailingWhiteSpace": true,
        "vim.smartRelativeLine": true,
        "vim.leader": "\\",
        "vim.normalModeKeyBindings": [
            {
                "before": [
                    "<leader>",
                    "f"
                ],
                "commands": [
                    "editor.action.formatDocument"
                ]
            },
        ],
        "vim.visualModeKeyBindings": [
            {
                "before": [
                    ">"
                ],
                "commands": [
                    "editor.action.indentLines"
                ]
            },
            {
                "before": [
                    "<"
                ],
                "commands": [
                    "editor.action.outdentLines"
                ]
            },
            {
                "before": [
                    "g",
                    "s"
                ],
                "commands": [
                    "editor.action.sortLinesAscending",
                    "extension.vim_escape",
                ]
            },
            {
                "before": [
                    "<leader>",
                    "f"
                ],
                "commands": [
                    "editor.action.formatSelection",
                    "extension.vim_escape",
                ]
            },
        ],
        "files.watcherExclude": {
            "**/.git/objects/**": true,
            "**/.git/subtree-cache/**": true,
            "**/node_modules/*/**": true
        },
    }
    ''
