#include"m.h"
#define K const

// exp, log, pow can be NULL on X86
TS JC {P ma;P free;P r;P xr;P e;P log;P pow;} JC;

U apple_compile(K JC*,K char*,S*,U*);

// NULL on error
// first argument: source
// second argument: error return
T apple_printty(K char*, T*);
T apple_dumpasm(K char*, T*);
T apple_x86(K char*, T*);
T apple_aarch64(K char*, T*);
T apple_dumpir(K char*, T*);
T apple_print_ts_sz(K char*, S*, T*);

TD apple_at{I_t=1,F_t=2,B_t=3} apple_at;

TD HK{Sc,Aa,Pi} HK;

TS apple_P{int pi_n; struct apple_t* a_pi;} apple_P;

// https://stackoverflow.com/questions/20752551/working-with-a-union-of-structs-in-c
TS apple_t {HK f; union {apple_at sa; apple_at aa; apple_P APi;} ty;} apple_t;

TS FnTy {int argc; apple_t* args; apple_t res;} FnTy;

// expect flat
_ void free_t(apple_t x){if(x.f==Pi){free(x.ty.APi.a_pi);}}

_ void freety(FnTy* x){DO(i,x->argc,free_t(x->args[i]));free(x->args);free_t(x->res);free(x);}

// NULL on error
FnTy* apple_ty(K char*, T*);
