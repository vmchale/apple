dyn.load("/usr/local/lib/libappler.dylib"); .Call("hs_init_R")
tyof<-function(a) {.Call("ty_R",a)}
asm<-function(a) {.Call("asm_R",a)}
jit<-function(a) {.Call("jit_R",a)}
run<-function(...) {.External("run_R",...)}
