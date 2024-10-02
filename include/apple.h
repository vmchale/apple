#include<stdint.h>
typedef intptr_t P;

// exp, log, pow can be NULL on X86
typedef struct JC {P ma; P free;P r;P xr;P e;P log;P pow;} JC;
static const JC sys={(P)&malloc,(P)&free,(P)&lrand48,(P)&drand48,(P)&exp,(P)&log,(P)&pow};

void* apple_compile(const JC*,const char*,size_t*,void**);

// NULL on error
// first argument: source
// second argument: error return
char* apple_printty(const char*, char**);
char* apple_dumpasm(const char*, char**);
char* apple_x86(const char*, char**);
char* apple_aarch64(const char*, char**);
char* apple_dumpir(const char*, char**);

enum apple_t{I_t,F_t,B_t,IA,FA,BA};

typedef struct FnTy {int argc; enum apple_t* args; enum apple_t res;} FnTy;

// NULL on error
FnTy* apple_ty(const char*, char**);
