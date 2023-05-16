# https://stackoverflow.com/questions/1902901/show-current-assembly-instruction-in-gdb
# https://stackoverflow.com/questions/55040576/how-to-change-the-disassembly-syntax-to-intel-using-gdb
set  disassemble-next-line on
show disassemble-next-line
set disassembly-flavor intel
layout asm
