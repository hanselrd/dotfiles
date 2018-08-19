set disassembly-flavor intel
layout split

set print pretty on
macro define offsetof(t, f) &((t *) 0)->f
