#include<R.h>
#include<Rinternals.h>
#include<HsFFI.h>
#include"../include/apple.h"

#define R return

// https://github.com/jsoftware/stats_jserver4r/blob/d7bfb7de28bd94e3c5f5e8284be2648f69ad1304/source/lib/util.c#L38
SEXP ss(char *s) {
    SEXP r;
    char *buf=strdup(s);
    PROTECT(r = allocVector(STRSXP, 1));
    SET_STRING_ELT(r, 0, mkChar(buf));
    free(buf);
    UNPROTECT(1);
    R r;
}

SEXP hs_init_R(void) {
    hs_init(0,0);
    R ss("...loaded GHC runtime");
}

SEXP ty_R(SEXP a) {
    char* err;char** err_p=&err;
    const char* inp=CHAR(STRING_ELT(a,0));
    char* ret=apple_printty(inp,err_p);
    if(ret==NULL) {
        SEXP ret=ss(err);free(err);
        R ret;
    }
    R ss(ret);
}
