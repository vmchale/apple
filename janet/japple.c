#include<janet.h>
#include<math.h>
#include<HsFFI.h>
#include<sys/mman.h>
#include"../c/ffi.c"

// Janet janet_wrap_array(JanetArray *x);
// Janet janet_wrap_number(double x);
// Janet janet_wrap_boolean(int x);
// Janet janet_wrap_integer(int32_t x);
// JanetArray *janet_unwrap_array(Janet x);
// int32_t janet_unwrap_integer(Janet x);
// int janet_unwrap_boolean(Janet x);
// double janet_unwrap_number(Janet x);
//
// JANET_API JanetArray *janet_array_n(const Janet *elements, int32_t n);
// For getting and setting values in the array, use array->data[index] directly.
//
// double janet_getnumber(const Janet *argv, int32_t n);
// JanetArray *janet_getarray(const Janet *argv, int32_t n);
// const char *janet_getcstring(const Janet *argv, int32_t n);
// int janet_getboolean(const Janet *argv, int32_t n);

typedef void* U;typedef size_t S;typedef double F;typedef int64_t J;

#define NIL janet_wrap_nil()

typedef struct Cache {U bc;S c_sz;FnTy* ty;U sa;ffi_cif* ffi;} Cache;

void freety(FnTy* x){free(x->args);free(x);}
static int jit_gc(void *data, size_t len) {
    Cache* j=(Cache*)data;
    munmap(j->bc,j->c_sz);
    free(j->sa);freety(j->ty);free(j->ffi);
    R 0;
}

static Janet apple_call(void *x, int32_t argc, Janet *argv) {
    Cache *jit = (Cache *)x;
    FnTy* ty=jit->ty;
    int aarg=ty->argc;
    janet_fixarity(argc, aarg);
    // void *janet_smalloc(size_t size);
    // void janet_sfree(void *p)
    U* vals=janet_smalloc(sizeof(U)*argc);
    U ret=janet_smalloc(8);
    Janet jarg;
    for(int k=0;k<aarg;k++){
        jarg=argv[k];
        Sw(ty->args[k]){
            C F_t: {F* xf=alloca(sizeof(F));xf[0]=janet_unwrap_number(jarg);vals[k]=xf;};BR
        }
    }
    U fp=jit->bc;ffi_cif* cif=jit->ffi;
    ffi_call(cif,fp,ret,vals);
    Janet r;
    Sw(ty->res){
        C F_t: r=janet_wrap_number(*(F*)ret);BR
    }
    janet_sfree(vals);janet_sfree(ret);
    R r;
}

static const JanetAbstractType jit_t = {
    .name = "jit",
    .gc = jit_gc,
    .gcmark = NULL,
    .get = NULL,
    .put = NULL,
    .marshal = NULL,
    .unmarshal = NULL,
    .tostring = NULL,
    .compare = NULL,
    .hash = NULL,
    .next = NULL,
    .call = apple_call,
    .length = NULL,
    .bytes = NULL,
};

static Cache *galloc_jit() {
  R (Cache*)janet_abstract(&jit_t, sizeof(Cache));
}

static Janet tyof_j(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    const char* inp=janet_getcstring(argv,0);
    char** e;
    char* o=apple_printty(inp, e);
    R janet_cstringv(o);
}

static Janet jit(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    const char* inp=janet_getcstring(argv,0);
    char*err; char** err_p=&err;
    FnTy* ty=apple_ty(inp,err_p);
    if(ty == NULL){
        printf("%s",err);
        free(err);R NIL;
    };
    U fp;S f_sz;U s;
    JC jc={(P)&malloc,(P)&free,(P)&lrand48,(P)&drand48,(P)&exp,(P)&log,(P)&pow};
    fp=apple_compile(&jc,inp,&f_sz,&s);
    Cache* j=galloc_jit();
    ffi_cif* ffi=apple_ffi(ty);
    j->bc=fp;j->c_sz=f_sz;j->ty=ty;j->sa=s;j->ffi=ffi;
    R janet_wrap_abstract(j);
    // janet_getabstract(argv, arg_ix, &jit_t);
}

static JanetReg cfuns[] = {
    {"tyof", tyof_j, "type of expression"},
    {"jit", jit, "JIT-compile source string"},
    {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
    hs_init(0,0);
    janet_cfuns(env, "apple", cfuns);
    janet_register_abstract_type(&jit_t);
}
