#include<sys/mman.h>
#include<R.h>
#include<Rinternals.h>
#include<HsFFI.h>
#include"../include/apple.h"
#include"../c/jit.h"

#define Sw switch
#define C case
#define BR break;

// asReal : SEXP -> double
// asInteger : SEXP -> int
// ScalarReal : double -> SEXP
// ScalarInteger : int -> SEXP
// SEXPTYPE = INTSXP | REALSXP

// http://adv-r.had.co.nz/C-interface.html

SEXP rf(U x) {
    I* i_p=x;
    I t=1;
    I rnk=i_p[0];
    SEXP dims=PROTECT(allocVector(INTSXP,(int)rnk));
    DO(i,rnk,t*=i_p[i+1];INTEGER(dims)[i]=(int)i_p[i+1]);
    SEXP ret=PROTECT(allocArray(REALSXP,dims));
    size_t sz=8*t;
    memcpy(REAL0(ret),i_p+rnk+1,sz);
    UNPROTECT(2);
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

SEXP apple_R(SEXP args) {
    char* err;char** err_p=&err;
    args=CDR(args);
    SEXP str=CAR(args);
    const char* inp=CHAR(asChar(str));
    FnTy* ty=apple_ty(inp,err_p);
    if(ty==NULL) {
        SEXP ret=mkString(err);free(err);
        R ret;
    }
    U fp; size_t f_sz;
    fp=apple_compile((P)&malloc,(P)&free,inp,&f_sz);
    U r;
    Sw(ty->res){
        C FA:
            Sw(ty->argc){
                C 0: {r=rf(((Ufp) fp)());BR}
            };BR
    }
    munmap(fp,f_sz);
    R r;
}
