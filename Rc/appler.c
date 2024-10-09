#include<sys/mman.h>
#include<R.h>
#include<Rinternals.h>
#include<HsFFI.h>
#include<ffi.h>
#include"../include/apple_abi.h"
#include"../c/ffi.c"

typedef size_t S;

// asReal : SEXP -> double
// asInteger : SEXP -> int
// ScalarReal : double -> SEXP
// ScalarInteger : int -> SEXP
// SEXPTYPE = INTSXP | REALSXP
// EXTPTR_PTR : SEXP -> void*

// http://adv-r.had.co.nz/C-interface.html

#define ERR(p,msg){if(p==NULL){SEXP er=mkString(msg);free(msg);R er;};}
#define DA(t,x,rnk,dims) J t=1;J* i_p=x;J rnk=i_p[0];SEXP dims=PROTECT(allocVector(INTSXP,(int)rnk));DO(i,rnk,t*=i_p[i+1];INTEGER(dims)[i]=(int)i_p[i+1]);

typedef struct AppleC {
    U code;S code_sz;FnTy* ty;U sa;ffi_cif* ffi;
} AppleC;

SEXP rf(U x) {
    DA(t,x,rnk,dims)
    SEXP ret=PROTECT(allocArray(REALSXP,dims));
    S sz=8*t;F* x_f=x;
    memcpy(REAL(ret),x_f+rnk+1,sz);
    UNPROTECT(2);
    R ret;
}

SEXP ri(U x) {
    DA(t,x,rnk,dims)
    SEXP ret=PROTECT(allocArray(INTSXP,dims));
    DO(i,t,INTEGER(ret)[i]=(int)i_p[i+rnk+1]);
    UNPROTECT(2);
    R ret;
}

SEXP rb(U x) {
    DA(t,x,rnk,dims)
    B* b_p=x+8*rnk+8;
    SEXP ret=PROTECT(allocArray(LGLSXP,dims));
    DO(i,t,LOGICAL(ret)[i]=(int)b_p[i]);
    UNPROTECT(2);
    R ret;
}

// vector only
U fr(SEXP x) {
    U ret;
    J dim=length(x);
    V(dim,REAL(x),ret);R ret;
}

U fi(SEXP x) {
    J rnk=1;J dim=length(x);
    J* ret=malloc(8*dim+16);
    ret[0]=rnk;ret[1]=dim;
    DO(i,dim,ret[i+2]=(J)(INTEGER(x)[i]));
    R ret;
}

U fb(SEXP x) {
    J rnk=1;J dim=length(x);
    B* ret=malloc(dim+16);
    J* i_p=ret;
    i_p[0]=rnk;i_p[1]=dim;
    DO(i,dim,ret[i+16]=(B)(LOGICAL(x)[i]));
    R ret;
}

SEXP hs_init_R(void) {
    hs_init(0,0);
    R mkString("...loaded GHC runtime");
}

SEXP ty_R(SEXP a) {
    char* err;char** err_p=&err;
    const char* inp=CHAR(asChar(a));
    char* typ=apple_printty(inp,err_p);
    ERR(typ,err);
    R mkString(typ);
}

SEXP jit_R(SEXP a){
    char* err; char** err_p=&err;
    const char* inp=CHAR(asChar(a));
    FnTy* ty=apple_ty(inp,err_p);
    ERR(ty,err);
    U fp;S f_sz;U s;
    fp=apple_compile(&sys,inp,&f_sz,&s);
    AppleC* rc=malloc(sizeof(AppleC));
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
    ERR(ret,err);
    R mkString(ret);
}

SEXP run_R(SEXP args){
    args=CDR(args);
    SEXP rc=CAR(args);
    AppleC* c=(AppleC*)(R_ExternalPtrAddr(rc));
    FnTy* ty=c->ty;U fp=c->code;ffi_cif* cif=c->ffi;
    SEXP r;
    int argc=ty->argc;
    U* vals=alloca(sizeof(U)*argc);U ret=alloca(8);
    uint8_t fs=0;
    for(int k=0;k<argc;k++){
        args=CDR(args);SEXP arg=CAR(args);
        Sw(ty->args[k]){
            C(FA, U* x=alloca(sizeof(U));x[0]=fr(arg);fs|=1<<k;vals[k]=x;)
            C(IA, U* x=alloca(sizeof(U));x[0]=fi(arg);fs|=1<<k;vals[k]=x;)
            C(BA, U* x=alloca(sizeof(U));x[0]=fb(arg);fs|=1<<k;vals[k]=x;)
            C(F_t, F* xf=alloca(sizeof(F));xf[0]=asReal(arg);vals[k]=xf;)
            C(I_t, J* xi=alloca(sizeof(J));xi[0]=(J)asInteger(arg);vals[k]=xi;)
        }
    }
    ffi_call(cif,fp,ret,vals);
    DO(i,argc,if(fs>>i&1){free(*(U*)vals[i]);})
    Sw(ty->res){
        C(FA,r=rf(*(U*)ret))
        C(IA,r=ri(*(U*)ret))
        C(BA,r=rb(*(U*)ret))
        C(F_t,r=ScalarReal(*(F*)ret))
        C(I_t,r=ScalarInteger((int)(*(J*)ret)))
        C(B_t,r=ScalarLogical(*(int*)ret))
    }
    R r;
}
