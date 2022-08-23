let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let Font/toMetadata = ../../../Lib/Font/toMetadata.partial.dhall

let env = ../../../codegen/environment.partial.dhall

let fontMetadata = Font/toMetadata env.font

in  ''
    # DO NOT EDIT! This file will be overwritten by LXAppearance.
    # Any customization should be done in ${Directory/toText
                                              Directory.Gtk1}/.gtkrc-2.0.mine instead.

    gtk-theme-name="Numix"
    gtk-icon-theme-name="Numix-Circle"
    gtk-font-name="${fontMetadata.windowManager.name} ${External/Prelude.Text.default
                                                          fontMetadata.windowManager.style} ${External/Prelude.Natural.show
                                                                                                fontMetadata.windowManager.size}"
    gtk-cursor-theme-name="Adwaita"
    gtk-cursor-theme-size=0
    gtk-toolbar-style=GTK_TOOLBAR_BOTH
    gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
    gtk-button-images=0
    gtk-menu-images=0
    gtk-enable-event-sounds=0
    gtk-enable-input-feedback-sounds=0
    gtk-xft-antialias=1
    gtk-xft-hinting=1
    gtk-xft-hintstyle="hintmedium"
    ''
