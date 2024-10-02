#include<janet.h>
#include<math.h>
#include<HsFFI.h>
#include<sys/mman.h>
#include"../c/ffi.c"

// Janet janet_wrap_array(JanetArray *x);
// Janet janet_wrap_boolean(int x);
// JanetArray *janet_unwrap_array(Janet x);
// int janet_unwrap_boolean(Janet x);
// JanetArray *janet_getarray(const Janet *argv, int32_t n);
// int janet_getboolean(const Janet *argv, int32_t n);
//
// JANET_API JanetArray *janet_array_n(const Janet *elements, int32_t n);
// For getting and setting values in the array, use array->data[index] directly.

typedef void* U;typedef size_t S;typedef double F;typedef int64_t J;

#define NIL janet_wrap_nil()

typedef struct JF {U bc;S c_sz;FnTy* ty;U sa;ffi_cif* ffi;} JF;

void freety(FnTy* x){free(x->args);free(x);}
static int jit_gc(void *data, size_t len) {
    JF* j=(JF*)data;
    munmap(j->bc,j->c_sz);
    free(j->sa);freety(j->ty);free(j->ffi);
    R 0;
}

U f_jv(JanetArray* x) {
    J n=(J)x->count;
    J sz_i=n+2;S sz=sz_i*8;
    U y=malloc(sz);J* x_i=y; F* x_f=y;
    x_i[0]=1;x_i[1]=n;
    Janet* js=x->data;
    DO(i,n,x_f[i+2]=janet_unwrap_number(js[i]));
    R y;
}

static Janet apple_call(void *x, int32_t argc, Janet *argv) {
    JF *jit = (JF *)x;
    FnTy* ty=jit->ty;
    int aarg=ty->argc;
    janet_fixarity(argc, aarg);
    U* vals=janet_smalloc(sizeof(U)*argc);U ret=janet_smalloc(8);
    for(int k=0;k<aarg;k++){
        Sw(ty->args[k]){
            C F_t: {F* xf=alloca(sizeof(F));xf[0]=janet_getnumber(argv,k);vals[k]=xf;};BR
            C I_t: {J* xi=alloca(sizeof(J));xi[0]=(J)janet_getinteger(argv,k);vals[k]=xi;};BR
            C FA: {U* a=alloca(sizeof(U));a[0]=f_jv(janet_getarray(argv,k));vals[k]=a;}BR
        }
    }
    U fp=jit->bc;ffi_cif* cif=jit->ffi;
    ffi_call(cif,fp,ret,vals);
    Janet r;
    Sw(ty->res){
        C F_t: r=janet_wrap_number(*(F*)ret);BR
        C I_t: r=janet_wrap_integer((int32_t)*(J*)ret);BR
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

static JF *galloc_jit() {
  R (JF*)janet_abstract(&jit_t, sizeof(JF));
}

static Janet tyof_j(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    const char* inp=janet_getcstring(argv,0);
    char** e;
    char* o=apple_printty(inp, e);
    R janet_cstringv(o);
}

static const JC jc={(P)&malloc,(P)&free,(P)&lrand48,(P)&drand48,(P)&exp,(P)&log,(P)&pow};

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
    fp=apple_compile(&sys,inp,&f_sz,&s);
    JF* j=galloc_jit();
    ffi_cif* ffi=apple_ffi(ty);
    j->bc=fp;j->c_sz=f_sz;j->ty=ty;j->sa=s;j->ffi=ffi;
    R janet_wrap_abstract(j);
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
