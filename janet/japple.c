#include<janet.h>
#include<HsFFI.h>
#include<sys/mman.h>
#include"../c/ffi.c"

// int janet_getboolean(const Janet *argv, int32_t n);

typedef void* U;typedef size_t S;typedef double F;typedef int64_t J;typedef uint8_t B;

#define NIL janet_wrap_nil()
#define ERR(p,msg){if(p==NULL){printf("%s\n",msg);free(msg);R NIL;};}
#define JA(n,xs) JanetArray* arr=janet_array((int32_t)n);arr->count=n;Janet* xs=arr->data;

typedef struct JF {U bc;S c_sz;FnTy* ty;U sa;ffi_cif* ffi;} JF;

void freety(FnTy* x){free(x->args);free(x);}
static int jit_gc(void *data, size_t len) {
    JF* j=(JF*)data;
    munmap(j->bc,j->c_sz);
    free(j->sa);freety(j->ty);free(j->ffi);
    R 0;
}

U fv_j(JanetArray* x) {
    J n=(J)x->count;
    J sz_i=n+2;S sz=sz_i*8;
    U y=malloc(sz);J* x_i=y;
    F* x_f=y;
    x_i[0]=1;x_i[1]=n;
    Janet* js=x->data;
    DO(i,n,x_f[i+2]=janet_unwrap_number(js[i]));
    R y;
}

U fv_i(JanetArray* x) {
    J n=(J)x->count;
    J sz_i=n+2;S sz=sz_i*8;
    U y=malloc(sz);J* x_i=y;
    x_i[0]=1;x_i[1]=n;
    Janet* js=x->data;
    DO(i,n,x_i[i+2]=(J)janet_unwrap_integer(js[i]));
    R y;
}

U fv_b(JanetArray* x) {
    J n=(J)x->count;
    S sz=n+16;
    U y=malloc(sz);J* x_i=y; B* x_b=y+16;
    x_i[0]=1;x_i[1]=n;
    Janet* js=x->data;
    DO(i,n,x_b[i]=janet_unwrap_boolean(js[i]));
    R y;
}

JanetArray* j_vb(U x) {
    J* i_p=x; B* b_p=x+16;
    J n=i_p[1];
    JA(n,xs)
    DO(j,n,xs[j]=janet_wrap_boolean((int32_t)b_p[j]));
    free(x);R arr;
}

JanetArray* j_vf(U x) {
    J* i_p=x;J n=i_p[1];F* f_p=x;
    JA(n,xs)
    DO(j,n,xs[j]=janet_wrap_number(f_p[j+2]));
    free(x);R arr;
}

JanetArray* j_vi(U x) {
    J* i_p=x;J n=i_p[1];
    JA(n,xs)
    DO(j,n,xs[j]=janet_wrap_integer((int32_t)i_p[j+2]));
    free(x);R arr;
}

static Janet apple_call(void *x, int32_t argc, Janet *argv) {
    JF *jit = (JF *)x;
    FnTy* ty=jit->ty;
    int aarg=ty->argc;
    janet_fixarity(argc, aarg);
    U* vals=janet_smalloc(sizeof(U)*argc);U ret=janet_smalloc(8);
    uint8_t fs=0;
    for(int k=0;k<aarg;k++){
        Sw(ty->args[k]){
            C F_t: {F* xf=alloca(sizeof(F));xf[0]=janet_getnumber(argv,k);vals[k]=xf;};BR
            C I_t: {J* xi=alloca(sizeof(J));xi[0]=(J)janet_getinteger(argv,k);vals[k]=xi;};BR
            C FA: {U* a=alloca(sizeof(U));a[0]=fv_j(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;}BR
            C IA: {U* a=alloca(sizeof(U));a[0]=fv_i(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;}BR
            C BA: {U* a=alloca(sizeof(U));a[0]=fv_b(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;}BR
        }
    }
    U fp=jit->bc;ffi_cif* cif=jit->ffi;
    ffi_call(cif,fp,ret,vals);
    Janet r;
    Sw(ty->res){
        C F_t: r=janet_wrap_number(*(F*)ret);BR
        C I_t: r=janet_wrap_integer((int32_t)*(J*)ret);BR
        C B_t: r=janet_wrap_boolean(*(int*)ret);BR
        C FA: r=janet_wrap_array(j_vf(*(U*)ret));BR
        C IA: r=janet_wrap_array(j_vi(*(U*)ret));BR
        C BA: r=janet_wrap_array(j_vb(*(U*)ret));BR
    }
    DO(i,argc,if(fs>>i&1){free(*(U*)vals[i]);})
    janet_sfree(vals);janet_sfree(ret);
    R r;
}

static const JanetAbstractType jit_t = {
    .name = "jit",
    .gc = jit_gc,
    .gcmark = NULL,
    .get = NULL, .put = NULL,
    .marshal = NULL, .unmarshal = NULL,
    .tostring = NULL,
    .compare = NULL,
    .hash = NULL,
    .next = NULL,
    .call = apple_call,
    .length = NULL,
    .bytes = NULL,
};

static JF *galloc_jit() {R (JF*)janet_abstract(&jit_t, sizeof(JF));}

static Janet tyof_j(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    const char* inp=janet_getcstring(argv,0);
    char* e;char** e_p=&e;
    char* o=apple_printty(inp, e_p);
    ERR(o,e)
    R janet_cstringv(o);
}

static Janet jit(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    const char* inp=janet_getcstring(argv,0);
    char*err; char** err_p=&err;
    FnTy* ty=apple_ty(inp,err_p);
    ERR(ty,err)
    U fp;S f_sz;U s;
    fp=apple_compile(&sys,inp,&f_sz,&s);
    JF* j=galloc_jit();
    ffi_cif* ffi=apple_ffi(ty);
    j->bc=fp;j->c_sz=f_sz;j->ty=ty;j->sa=s;j->ffi=ffi;
    R janet_wrap_abstract(j);
}

static JanetReg cfuns[] = {
    {"tyof", tyof_j, "type of expression"},
    {"jit", jit, "Compile source string into Janet callable"},
    {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
    hs_init(0,0);
    janet_cfuns(env, "apple", cfuns);
    janet_register_abstract_type(&jit_t);
}
