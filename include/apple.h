void* apple_compile(const char*);
// NULL on error
char* apple_printty(const char*, char**);

enum apple_t{I_t,F_t,IA,FA,Fn};

typedef struct FnTy {int argc; enum apple_t* args; enum apple_t res;} FnTy;

// returns -1 on error
enum apple_t apple_ty(const char*, char**, FnTy**);
