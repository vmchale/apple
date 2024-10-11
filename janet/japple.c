#include<janet.h>
#include<HsFFI.h>
#include<sys/mman.h>
#include"../c/ffi.c"

// int janet_getboolean(const Janet *argv, int32_t n);

typedef void* U;typedef size_t S;typedef double F;typedef int64_t J;typedef uint8_t B;typedef char* T;

#define ZU static U
#define Z static

#define NIL janet_wrap_nil()
#define ERR(p,msg){if(p==NULL){printf("%s\n",msg);free(msg);R NIL;};}
#define JA(x,n,xs) J n=((J*)x)[1];JanetArray* arr=janet_array((int32_t)n);arr->count=n;Janet* xs=arr->data;
#define VA(sz,y) U y=malloc(sz);{J* x_i=y;x_i[0]=1;x_i[1]=n;}
#define L(a) (J)a->count

typedef struct JF {U bc;S c_sz;FnTy* ty;U sa;ffi_cif* ffi; T ts;} JF;

void freety(FnTy* x){free(x->args);free(x);}
static int jit_gc(void *data, size_t len) {
    JF* j=(JF*)data;
    munmap(j->bc,j->c_sz);
    free(j->sa);freety(j->ty);free(j->ffi);free(j->ts);
    R 0;
}

ZU fv_j(JanetArray* x) {
    J n=L(x);
    VA(n*8+16,y);
    F* x_f=y;
    Janet* js=x->data;
    DO(i,n,x_f[i+2]=janet_unwrap_number(js[i]));
    R y;
}

ZU fv_i(JanetArray* x) {
    J n=L(x);
    VA(n+8+16,y);
    J* x_i=y;
    Janet* js=x->data;
    DO(i,n,x_i[i+2]=(J)janet_unwrap_integer(js[i]));
    R y;
}

ZU fv_b(JanetArray* x) {
    J n=L(x);
    VA(n+16,y);
    B* x_b=y+16;
    Janet* js=x->data;
    DO(i,n,x_b[i]=janet_unwrap_boolean(js[i]));
    R y;
}

Z JanetArray* j_vb(U x) {
    JA(x,n,xs)
    B* b_p=x+16;
    DO(j,n,xs[j]=janet_wrap_boolean((int32_t)b_p[j]));
    free(x);R arr;
}

Z JanetArray* j_vf(U x) {
    JA(x,n,xs)
    F* f_p=x;
    DO(j,n,xs[j]=janet_wrap_number(f_p[j+2]));
    free(x);R arr;
}

Z JanetArray* j_vi(U x) {
    JA(x,n,xs)
    J* i_p=x;
    DO(j,n,xs[j]=janet_wrap_integer((int32_t)i_p[j+2]));
    free(x);R arr;
}

Z Janet apple_call(void *x, int32_t argc, Janet *argv) {
    JF *jit = (JF *)x;
    FnTy* ty=jit->ty;
    int aarg=ty->argc;
    janet_fixarity(argc, aarg);
    U* vals=janet_smalloc(sizeof(U)*argc);U ret=janet_smalloc(8);
    uint8_t fs=0;
    for(int k=0;k<aarg;k++){
        switch(ty->args[k]){
            C(F_t, F* xf=alloca(sizeof(F));xf[0]=janet_getnumber(argv,k);vals[k]=xf;)
            C(I_t, J* xi=alloca(sizeof(J));xi[0]=(J)janet_getinteger(argv,k);vals[k]=xi;)
            C(FA, U* a=alloca(sizeof(U));a[0]=fv_j(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;)
            C(IA, U* a=alloca(sizeof(U));a[0]=fv_i(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;)
            C(BA, U* a=alloca(sizeof(U));a[0]=fv_b(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;)
        }
    }
    U fp=jit->bc;ffi_cif* cif=jit->ffi;
    ffi_call(cif,fp,ret,vals);
    Janet r;
    switch(ty->res){
        C(F_t, r=janet_wrap_number(*(F*)ret))
        C(I_t, r=janet_wrap_integer((int32_t)*(J*)ret))
        C(B_t, r=janet_wrap_boolean(*(int*)ret))
        C(FA, r=janet_wrap_array(j_vf(*(U*)ret)))
        C(IA, r=janet_wrap_array(j_vi(*(U*)ret)))
        C(BA, r=janet_wrap_array(j_vb(*(U*)ret)))
    }
    DO(i,argc,if(fs>>i&1){free(*(U*)vals[i]);})
    janet_sfree(vals);janet_sfree(ret);
    R r;
}

Z void jit_ts(void* jit, JanetBuffer* buf) {JF* c=(JF*)jit;janet_buffer_push_cstring(buf,c->ts);}

static const JanetAbstractType jit_t = {
    .name = "jit",
    .gc = jit_gc,
    .gcmark = NULL,
    .get = NULL, .put = NULL,
    .marshal = NULL, .unmarshal = NULL,
    .tostring = jit_ts,
    .compare = NULL,
    .hash = NULL,
    .next = NULL,
    .call = apple_call,
    .length = NULL,
    .bytes = NULL,
};

static JF *galloc_jit() {R (JF*)janet_abstract(&jit_t, sizeof(JF));}

Z Janet tyof_j(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    const char* inp=janet_getcstring(argv,0);
    T e;
    T o=apple_printty(inp,&e);
    ERR(o,e)
    R janet_cstringv(o);
}

Z Janet jit(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    const char* inp=janet_getcstring(argv,0);
    T err;
    FnTy* ty=apple_ty(inp,&err);
    ERR(ty,err);
    T tystr=apple_printty(inp,&err);
    S f_sz;U s;
    U fp=apple_compile(&sys,inp,&f_sz,&s);
    JF* j=galloc_jit();
    ffi_cif* ffi=apple_ffi(ty);
    j->bc=fp;j->c_sz=f_sz;j->ty=ty;j->sa=s;j->ffi=ffi;j->ts=tystr;
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
