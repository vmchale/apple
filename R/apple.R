dyn.load("/usr/local/lib/libappler.so"); .Call("hs_init_R")
tyof=function(a) {.Call("ty_R",a)}
asm=function(a) {.Call("asm_R",a)}
apple=function(...) {.External("apple_R",...)}
