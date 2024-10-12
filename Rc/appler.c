#include<sys/mman.h>
#include<R.h>
#include<Rinternals.h>
#include<HsFFI.h>
#include<ffi.h>
#include"../include/apple_abi.h"
#include"../c/ffi.c"

typedef size_t S;typedef char* T;

// asReal : SEXP -> double
// asInteger : SEXP -> int
// ScalarReal : double -> SEXP
// ScalarInteger : int -> SEXP
// SEXPTYPE = INTSXP | REALSXP | LGLSXP
// EXTPTR_PTR : SEXP -> void*

// http://adv-r.had.co.nz/C-interface.html

#define ERR(p,msg){if(p==NULL){SEXP er=mkString(msg);free(msg);R er;};}
#define DA(n,x,rnk,t,ra) J* i_p=x;J rnk=i_p[0];SEXP ds=PROTECT(allocVector(INTSXP,(int)rnk));J n=1;DO(i,rnk,n*=i_p[i+1];INTEGER(ds)[i]=(int)i_p[i+1]);SEXP ra=PROTECT(allocArray(t,ds));

#define Z static
#define ZU static U
#define ZR static SEXP

typedef struct AppleC {U code;S code_sz;FnTy* ty;U sa;ffi_cif* ffi;} AppleC;

Z void freety(FnTy* x){free(x->args);free(x);}
Z void clear(SEXP jit) {
    AppleC* c=(AppleC*)R_ExternalPtrAddr(jit);
    munmap(c->code,c->code_sz);
    free(c->sa);free(c->ffi);freety(c->ty);
}

ZR rf(U x) {DA(n,x,rnk,REALSXP,r);F* x_f=x;memcpy(REAL(r),x_f+rnk+1,n*8);UNPROTECT(2);R r;}
ZR ri(U x) {DA(n,x,rnk,INTSXP,r);DO(i,n,INTEGER(r)[i]=(int)i_p[i+rnk+1]);UNPROTECT(2);R r;}
ZR rb(U x) {DA(n,x,rnk,LGLSXP,r);B* b_p=x+8*rnk+8;DO(i,n,LOGICAL(r)[i]=(int)b_p[i]);UNPROTECT(2);R r;}

// vector only
ZU fr(SEXP x) {U ret;J dim=length(x);V(dim,REAL(x),ret);R ret;}
ZU fi(SEXP x) {J dim=length(x);J* ret=malloc(8*dim+16);J rnk=1;ret[0]=rnk;ret[1]=dim;DO(i,dim,ret[i+2]=(J)(INTEGER(x)[i]));R ret;}
ZU fb(SEXP x) {J dim=length(x);B* ret=malloc(dim+16);J* i_p=(J*)ret;J rnk=1;i_p[0]=rnk;i_p[1]=dim;DO(i,dim,ret[i+16]=(B)(LOGICAL(x)[i]));R ret;}

SEXP hs_init_R(void) {
    hs_init(0,0);
    R mkString("...loaded GHC runtime");
}

SEXP ty_R(SEXP a) {
    const char* inp=CHAR(asChar(a));T err;
    T typ=apple_printty(inp,&err);
    ERR(typ,err);
    R mkString(typ);
}

SEXP jit_R(SEXP a){
    const char* inp=CHAR(asChar(a));
    T err;
        FnTy* ty=apple_ty(inp,&err);
    ERR(ty,err);
    S f_sz;U s;
    U fp=apple_compile(&sys,inp,&f_sz,&s);
    AppleC* rc=malloc(sizeof(AppleC));
    ffi_cif* ffi=apple_ffi(ty);
    rc->code=fp;rc->code_sz=f_sz;rc->ty=ty;rc->sa=s;rc->ffi=ffi;
    // http://homepage.divms.uiowa.edu/~luke/R/simpleref.html#toc6
    // https://github.com/hadley/r-internals/blob/master/external-pointers.md
    SEXP r=R_MakeExternalPtr((U)rc,R_NilValue,R_NilValue);
    R_RegisterCFinalizer(r,&clear);
    R r;
}

SEXP asm_R(SEXP a) {
    const char* inp=CHAR(asChar(a));T err;
    T ret=apple_dumpasm(inp,&err);
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
        switch(ty->args[k]){
            C(FA, U* x=alloca(sizeof(U));x[0]=fr(arg);fs|=1<<k;vals[k]=x;)
            C(IA, U* x=alloca(sizeof(U));x[0]=fi(arg);fs|=1<<k;vals[k]=x;)
            C(BA, U* x=alloca(sizeof(U));x[0]=fb(arg);fs|=1<<k;vals[k]=x;)
            C(F_t, F* xf=alloca(sizeof(F));xf[0]=asReal(arg);vals[k]=xf;)
            C(I_t, J* xi=alloca(sizeof(J));xi[0]=(J)asInteger(arg);vals[k]=xi;)
        }
    }
    ffi_call(cif,fp,ret,vals);
    DO(i,argc,if(fs>>i&1){free(*(U*)vals[i]);})
    switch(ty->res){
        C(FA,r=rf(*(U*)ret))
        C(IA,r=ri(*(U*)ret))
        C(BA,r=rb(*(U*)ret))
        C(F_t,r=ScalarReal(*(F*)ret))
        C(I_t,r=ScalarInteger((int)(*(J*)ret)))
        C(B_t,r=ScalarLogical(*(int*)ret))
    }
    R r;
}
