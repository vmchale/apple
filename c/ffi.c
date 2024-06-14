#include<stdlib.h>
#include<ffi.h>
#include"../include/apple.h"

#define R return
#define Sw switch
#define C case
#define BR break;

#define DO(i,n,a) {int i;for(i=0;i<n;i++){a;}}

#define F(r,t) {Sw(t){C I_t: r=&ffi_type_sint64;BR;C F_t: r=&ffi_type_double;BR;C FA: r=&ffi_type_pointer;BR;C IA: r=&ffi_type_pointer;BR}}

ffi_cif* apple_ffi(FnTy* ty) {
    ffi_cif* cif=malloc(sizeof(cif));
    int argc=ty->argc;
    // two arrays of floats segfaults when ffi_prep_cif is called
    ffi_type** args=malloc(sizeof(ffi_type*)*(argc+1));
    enum apple_t* argv=ty->args;
    DO(i,argc,F(args[i],argv[i]))
    ffi_type* ret;F(ret,ty->res);
    ffi_prep_cif(cif,FFI_DEFAULT_ABI,(unsigned int)argc,ret,args);
    R cif;
}
