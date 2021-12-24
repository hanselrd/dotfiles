let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

in  ''
    set disassembly-flavor intel
    #layout split

    source ${Directory/toText Directory.Gdb2}/offsetsof.py

    set print pretty on
    macro define offsetof(t, f) (int)&((t *) 0)->f
    ''
