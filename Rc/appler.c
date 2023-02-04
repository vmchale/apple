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
    memcpy(REAL(ret),i_p+rnk+1,sz);
    UNPROTECT(2);
    R ret;
}

// vector only
U fr(SEXP x) {
    I rnk=1;I dim=length(x);
    I* ret=malloc(8*(2+dim));
    ret[0]=rnk;ret[1]=dim;
    memcpy(ret+2,REAL(x),dim*8);
    R ret;
}

void freety(FnTy* x){free(x->args);free(x);}

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
                C 1:
                    Sw(ty->args[0]){
                        C FA: {SEXP arg0=CADR(args);r=rf(((Aafp) fp)(fr(arg0)));BR}
                    }
            };BR
        C F_t:
            Sw(ty->argc){
                C 0: {r=ScalarReal(((Ffp) fp)());BR}
                C 1:
                    Sw(ty->args[0]){
                        C F_t: {SEXP arg0=CADR(args);r=ScalarReal(((Fffp) fp)(asReal(arg0)));BR}
                    }BR;
                C 2:
                    Sw(ty->args[0]){
                        C F_t: {
                            Sw(ty->args[1]) {
                                C F_t: {SEXP arg0=CADR(args);SEXP arg1=CADDR(args);r=ScalarReal(((Ffffp) fp)(asReal(arg0),asReal(arg1)));BR}
                            }BR;
                        }
                    }BR;
            };BR
    }
    freety(ty);
    munmap(fp,f_sz);
    R r;
}
