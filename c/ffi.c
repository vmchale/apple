#include<stdlib.h>
#include<math.h>
#include<ffi.h>
#include"../include/apple.h"

#define R return
// https://github.com/tlack/b-decoded/blob/3c21a33a5c3f5d39f75014e10f875fe830a8b326/orig-files/c.h#L8
#define C(i,a) case i:{a;}break;

#define DO(i,n,a) {int i;for(i=0;i<n;i++){a;}}

#define F(r,t) {switch(t){C(I_t,r=&ffi_type_sint64) C(F_t,r=&ffi_type_double) C(FA,r=&ffi_type_pointer) C(IA,r=&ffi_type_pointer) C(BA,r=&ffi_type_pointer)}}

static const JC sys={(P)&malloc,(P)&free,(P)&lrand48,(P)&drand48,(P)&exp,(P)&log,(P)&pow};

ffi_cif* apple_ffi(FnTy* ty) {
    ffi_cif* cif=malloc(sizeof(*cif));
    int argc=ty->argc;
    ffi_type** args=malloc(sizeof(ffi_type*)*argc);
    enum apple_t* argv=ty->args;
    DO(i,argc,F(args[i],argv[i]))
    ffi_type* ret;F(ret,ty->res);
    ffi_prep_cif(cif,FFI_DEFAULT_ABI,(unsigned int)argc,ret,args);
    R cif;
}
