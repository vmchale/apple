#include<stdint.h>
typedef intptr_t P;

// first argument: pointer to malloc
// second argument: pointer to free
// third argument: pointer to drand48
// third argument: pointer to exp (can be NULL on X86)
// fourth argument: pointer to log (can be NULL on X86)
// fifth argument: pointer to pow (can be NULL on X86)
void* apple_compile(P,P,P,P,P,P,const char*,size_t*,void**);

// NULL on error
// first argument: source
// second argument: error return
char* apple_printty(const char*, char**);
char* apple_dumpasm(const char*, char**);
char* apple_x86(const char*, char**);
char* apple_aarch64(const char*, char**);
char* apple_dumpir(const char*, char**);

enum apple_t{I_t,F_t,IA,FA};

typedef struct FnTy {int argc; enum apple_t* args; enum apple_t res;} FnTy;

// NULL on error
FnTy* apple_ty(const char*, char**);
