{
  lib,
  pkgs,
  ...
}: {
  alacrittyThemeFromScheme = {scheme}:
    with scheme.palette; {
      primary = {
        background = "#${base00}";
        foreground = "#${base05}";
      };
      cursor = {
        text = "#${base00}";
        cursor = "#${base05}";
      };
      normal = {
        black = "#${base00}";
        red = "#${base08}";
        green = "#${base0B}";
        yellow = "#${base0A}";
        blue = "#${base0D}";
        magenta = "#${base0E}";
        cyan = "#${base0C}";
        white = "#${base05}";
      };
      bright = {
        black = "#${base03}";
        red = "#${base09}";
        green = "#${base01}";
        yellow = "#${base02}";
        blue = "#${base04}";
        magenta = "#${base06}";
        cyan = "#${base0F}";
        white = "#${base07}";
      };
    };

  batThemeFromScheme = {scheme}:
    with scheme.palette;
      pkgs.writeText "bat-theme-${scheme.slug}.tmTheme" ''
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0">
            <dict>
                <key>name</key>
                <string>nix-${scheme.slug}</string>
                <key>colorSpaceName</key>
                <string>sRGB</string>
                <key>settings</key>
                <array>
                    <dict>
                        <key>settings</key>
                        <dict>
                            <key>background</key>
                            <string>#${base00}</string>
                            <key>caret</key>
                            <string>#${base07}</string>
                            <key>foreground</key>
                            <string>#${base07}</string>
                            <key>invisibles</key>
                            <string>#${base08}</string>
                            <key>lineHighlight</key>
                            <string>#${base08}</string>
                            <key>selection</key>
                            <string>#${base0B}</string>
                            <key>gutter</key>
                            <string>#${base0A}</string>
                            <key>gutterForeground</key>
                            <string>#${base08}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Text</string>
                        <key>scope</key>
                        <string>variable.parameter.function</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base07}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Comments</string>
                        <key>scope</key>
                        <string>comment, punctuation.definition.comment</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base08}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Punctuation</string>
                        <key>scope</key>
                        <string>punctuation.definition.string, punctuation.definition.variable, punctuation.definition.string, punctuation.definition.parameters, punctuation.definition.string, punctuation.definition.array</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base07}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Delimiters</string>
                        <key>scope</key>
                        <string>none</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base07}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Operators</string>
                        <key>scope</key>
                        <string>keyword.operator</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base07}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Keywords</string>
                        <key>scope</key>
                        <string>keyword</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base05}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Variables</string>
                        <key>scope</key>
                        <string>variable</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base07}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Functions</string>
                        <key>scope</key>
                        <string>entity.name.function, meta.require, support.function.any-method</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base04}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Labels</string>
                        <key>scope</key>
                        <string>entity.name.label</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base0E}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Classes</string>
                        <key>scope</key>
                        <string>support.class, entity.name.class, entity.name.type.class</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base03}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Classes</string>
                        <key>scope</key>
                        <string>meta.class</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base0F}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Methods</string>
                        <key>scope</key>
                        <string>keyword.other.special-method</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base04}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Storage</string>
                        <key>scope</key>
                        <string>storage</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base05}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Support</string>
                        <key>scope</key>
                        <string>support.function</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base06}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Strings, Inherited Class</string>
                        <key>scope</key>
                        <string>string, constant.other.symbol, entity.other.inherited-class</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base02}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Integers</string>
                        <key>scope</key>
                        <string>constant.numeric</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Floats</string>
                        <key>scope</key>
                        <string>none</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Boolean</string>
                        <key>scope</key>
                        <string>none</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Constants</string>
                        <key>scope</key>
                        <string>constant</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Tags</string>
                        <key>scope</key>
                        <string>entity.name.tag</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base01}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Attributes</string>
                        <key>scope</key>
                        <string>entity.other.attribute-name</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Attribute IDs</string>
                        <key>scope</key>
                        <string>entity.other.attribute-name.id, punctuation.definition.entity</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base04}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Selector</string>
                        <key>scope</key>
                        <string>meta.selector</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base05}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Values</string>
                        <key>scope</key>
                        <string>none</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Headings</string>
                        <key>scope</key>
                        <string>markup.heading punctuation.definition.heading, entity.name.section</string>
                        <key>settings</key>
                        <dict>
                            <key>fontStyle</key>
                            <string></string>
                            <key>foreground</key>
                            <string>#${base04}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Units</string>
                        <key>scope</key>
                        <string>keyword.other.unit</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Bold</string>
                        <key>scope</key>
                        <string>markup.bold, punctuation.definition.bold</string>
                        <key>settings</key>
                        <dict>
                            <key>fontStyle</key>
                            <string>bold</string>
                            <key>foreground</key>
                            <string>#${base03}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Italic</string>
                        <key>scope</key>
                        <string>markup.italic, punctuation.definition.italic</string>
                        <key>settings</key>
                        <dict>
                            <key>fontStyle</key>
                            <string>italic</string>
                            <key>foreground</key>
                            <string>#${base05}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Code</string>
                        <key>scope</key>
                        <string>markup.raw.inline</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base02}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Link Text</string>
                        <key>scope</key>
                        <string>string.other.link, punctuation.definition.string.end.markdown, punctuation.definition.string.begin.markdown</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base01}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Link Url</string>
                        <key>scope</key>
                        <string>meta.link</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Quotes</string>
                        <key>scope</key>
                        <string>markup.quote</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base09}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Separator</string>
                        <key>scope</key>
                        <string>meta.separator</string>
                        <key>settings</key>
                        <dict>
                            <key>background</key>
                            <string>#${base0B}</string>
                            <key>foreground</key>
                            <string>#${base07}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Inserted</string>
                        <key>scope</key>
                        <string>markup.inserted</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base02}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Deleted</string>
                        <key>scope</key>
                        <string>markup.deleted</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base01}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Changed</string>
                        <key>scope</key>
                        <string>markup.changed</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base05}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Colors</string>
                        <key>scope</key>
                        <string>constant.other.color</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base06}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Regular Expressions</string>
                        <key>scope</key>
                        <string>string.regexp</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base06}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Escape Characters</string>
                        <key>scope</key>
                        <string>constant.character.escape</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base06}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Embedded</string>
                        <key>scope</key>
                        <string>punctuation.section.embedded, variable.interpolation</string>
                        <key>settings</key>
                        <dict>
                            <key>foreground</key>
                            <string>#${base05}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Illegal</string>
                        <key>scope</key>
                        <string>invalid.illegal</string>
                        <key>settings</key>
                        <dict>
                            <key>background</key>
                            <string>#${base01}</string>
                            <key>foreground</key>
                            <string>#${base0F}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Broken</string>
                        <key>scope</key>
                        <string>invalid.broken</string>
                        <key>settings</key>
                        <dict>
                            <key>background</key>
                            <string>#${base09}</string>
                            <key>foreground</key>
                            <string>#${base00}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Deprecated</string>
                        <key>scope</key>
                        <string>invalid.deprecated</string>
                        <key>settings</key>
                        <dict>
                            <key>background</key>
                            <string>#${base0E}</string>
                            <key>foreground</key>
                            <string>#${base0F}</string>
                        </dict>
                    </dict>
                    <dict>
                        <key>name</key>
                        <string>Unimplemented</string>
                        <key>scope</key>
                        <string>invalid.unimplemented</string>
                        <key>settings</key>
                        <dict>
                            <key>background</key>
                            <string>#${base08}</string>
                            <key>foreground</key>
                            <string>#${base0F}</string>
                        </dict>
                    </dict>
                </array>
                <key>uuid</key>
                <string>uuid</string>
            </dict>
        </plist>
      '';

  fzfThemeFromScheme = {scheme}:
    with scheme.palette; {
      bg = "#${base00}";
      "bg+" = "#${base01}";
      fg = "#${base04}";
      "fg+" = "#${base06}";
      hl = "#${base0D}";
      "hl+" = "#${base0D}";
      header = "#${base0D}";
      info = "#${base0A}";
      marker = "#${base0C}";
      pointer = "#${base0C}";
      prompt = "#${base0A}";
      spinner = "#${base0C}";
    };

  tmuxThemeFromScheme = {scheme}:
    with scheme.palette;
      pkgs.writeText "tmux-theme-${scheme.slug}.tmux" ''
        # default statusbar colors
        set-option -g status-style "fg=#${base04},bg=#${base01}"

        # default window title colors
        set-window-option -g window-status-style "fg=#${base04},bg=default"

        # active window title colors
        set-window-option -g window-status-current-style "fg=#${base0A},bg=default"

        # pane border
        set-option -g pane-border-style "fg=#${base01}"
        set-option -g pane-active-border-style "fg=#${base02}"

        # message text
        set-option -g message-style "fg=#${base05},bg=#${base01}"

        # pane number display
        set-option -g display-panes-active-colour "#${base0B}"
        set-option -g display-panes-colour "#${base0A}"

        # clock
        set-window-option -g clock-mode-colour "#${base0B}"

        # copy mode highligh
        set-window-option -g mode-style "fg=#${base04},bg=#${base02}"

        # bell
        set-window-option -g window-status-bell-style "fg=#${base01},bg=#${base08}"
      '';
}
