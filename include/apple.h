void* apple_compile(const char*);
// NULL on error
char* apple_printty(const char*, char**);

// FIXME: how do c-niles handle tuples??
enum apple_t{I_t,F_t,IA,FA,Fn};

// returns -1 on error
enum apple_t apple_ty(const char*, char**);

enum apple_err{TooPolymorphic,Wrong};
