set disassembly-flavor intel
#layout split

source ~/.gdb/offsetsof.py

set print pretty on
macro define offsetof(t, f) (int)&((t *) 0)->f
