#include<sys/mman.h>
#include<R.h>
#include<Rinternals.h>
#include<HsFFI.h>
#include<ffi.h>
#include"../include/apple_abi.h"
#include"../c/ffi.c"

typedef size_t S;

#define Sw switch
#define C case
#define BR break;

// asReal : SEXP -> double
// asInteger : SEXP -> int
// ScalarReal : double -> SEXP
// ScalarInteger : int -> SEXP
// SEXPTYPE = INTSXP | REALSXP
// EXTPTR_PTR : SEXP -> void*

// http://adv-r.had.co.nz/C-interface.html

typedef struct AppleCache {
    U code;S code_sz;FnTy* ty;U sa;ffi_cif* ffi;
} AppleCache;

SEXP rf(U x) {
    J* i_p=x;
    J t=1;
    J rnk=i_p[0];
    SEXP dims=PROTECT(allocVector(INTSXP,(int)rnk));
    DO(i,rnk,t*=i_p[i+1];INTEGER(dims)[i]=(int)i_p[i+1]);
    SEXP ret=PROTECT(allocArray(REALSXP,dims));
    S sz=8*t;
    memcpy(REAL(ret),i_p+rnk+1,sz);
    UNPROTECT(2);
    R ret;
}

// vector only
U fr(SEXP x) {
    J rnk=1;J dim=length(x);
    J* ret=malloc(8*(2+dim));
    ret[0]=rnk;ret[1]=dim;
    memcpy(ret+2,REAL(x),dim*8);
    R ret;
}

SEXP hs_init_R(void) {
    hs_init(0,0);
    R mkString("...loaded GHC runtime");
}

SEXP ty_R(SEXP a) {
    char* err;char** err_p=&err;
    const char* inp=CHAR(asChar(a));
    char* ret=apple_printty(inp,err_p);
    if(ret==NULL) {
        SEXP ret=mkString(err);free(err);
        R ret;
    }
    R mkString(ret);
}

SEXP jit_R(SEXP a){
    char* err; char** err_p=&err;
    const char* inp=CHAR(asChar(a));
    FnTy* ty=apple_ty(inp,err_p);
    if(ty == NULL){
        SEXP ret=mkString(err);free(err);
        R ret;
    };
    U fp;S f_sz;U s;
    fp=apple_compile((P)&malloc,(P)&free,(P)&exp,(P)&log,(P)&exp,inp,&f_sz,&s);
    AppleCache* rc=malloc(sizeof(AppleCache));
    ffi_cif* ffi=apple_ffi(ty);
    rc->code=fp;rc->code_sz=f_sz;rc->ty=ty;rc->sa=s;rc->ffi=ffi;
    // http://homepage.divms.uiowa.edu/~luke/R/simpleref.html
    // https://github.com/hadley/r-internals/blob/master/external-pointers.md
    SEXP r=R_MakeExternalPtr((U)rc,R_NilValue,R_NilValue);
    R r;
}

SEXP asm_R(SEXP a) {
    char* err; char** err_p=&err;
    const char* inp=CHAR(asChar(a));
    char* ret=apple_dumpasm(inp,err_p);
    if(ret==NULL) {
        SEXP ret=mkString(err);free(err);
        R ret;
    }
    R mkString(ret);
}

SEXP run_R(SEXP args){
    args=CDR(args);
    SEXP rc=CAR(args);
    AppleCache* c=(AppleCache*)(R_ExternalPtrAddr(rc));
    FnTy* ty=c->ty;U fp=c->code;ffi_cif* cif=c->ffi;
    SEXP r;
    int argc=ty->argc;
    U* vals=alloca(sizeof(U)*argc);
    U ret=alloca(8);
    for(int k=0;k<argc;k++){
        args=CDR(args);SEXP arg=CAR(args);
        Sw(ty->args[k]){
            C FA: {U* x=alloca(sizeof(U));x[0]=fr(arg);vals[k]=x;};BR
            C F_t: {F* xf=alloca(sizeof(F));xf[0]=asReal(arg);vals[k]=xf;};BR
            C I_t: {J* xi=alloca(sizeof(J));xi[0]=(int64_t)asInteger(arg);vals[k]=xi;};BR
        }
    }
    ffi_call(cif,fp,ret,vals);
    Sw(ty->res){
        C FA: r=rf(*(U*)ret);BR
        C F_t: r=ScalarReal(*(F*)ret);BR
        C I_t: r=ScalarInteger((int)(*(J*)ret));BR
    }
    R r;
}
