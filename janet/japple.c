#include<janet.h>
#include<HsFFI.h>
#include<sys/mman.h>
#include"../c/ffi.c"

#define NIL janet_wrap_nil()
#define ERR(p,msg) if(p==NULL){printf("%s\n",msg);free(msg);R NIL;}
#define JA J n=((J*)x)[1];JanetArray* arr=janet_array((int32_t)n);arr->count=n;Janet* xs=arr->data;
#define VA(sz) U y=malloc(sz+16);{J* x_i=y;x_i[0]=1;x_i[1]=n;}
#define L(a) (J)a->count

TS JF {U bc;S c_sz;FnTy* ty;U sa;ffi_cif* ffi;T ts;} JF;

#define nyi {printf("unsupported: tuple in bindings.");R NIL;}

_ int jit_gc(U data, size_t len) {
    JF* j=(JF*)data;
    munmap(j->bc,j->c_sz);
    free(j->sa);freety(j->ty);free(j->ffi);free(j->ts);
    R 0;
}

_ U fv_r(K JanetArray* x) {J n=L(x);VA(n*8);F* x_f=y;Janet* js=x->data;iX(n,x_f[i+2]=janet_unwrap_number(js[i]));R y;}
_ U fv_i(K JanetArray* x) {J n=L(x);VA(n*8);J* x_i=y;Janet* js=x->data;iX(n,x_i[i+2]=(J)janet_unwrap_integer(js[i]));R y;}
_ U fv_b(K JanetArray* x) {J n=L(x);VA(n);B* x_b=y+16;Janet* js=x->data;iX(n,x_b[i]=janet_unwrap_boolean(js[i]));R y;}

_ JanetArray* j_vb(K U x) {JA;B* b_p=x+16;iX(n,xs[i]=janet_wrap_boolean((int32_t)b_p[i]));free(x);R arr;}
_ JanetArray* j_vf(K U x) {JA;F* f_p=x;iX(n,xs[i]=janet_wrap_number(f_p[i+2]));free(x);R arr;}
_ JanetArray* j_vi(K U x) {JA;J* i_p=x;iX(n,xs[i]=janet_wrap_integer((int32_t)i_p[i+2]));free(x);R arr;}

Z Janet jr(K apple_t, K U);

Z Janet jr(K apple_t t, K U x){
    Janet r;
    ArgTy(t,
        r=janet_wrap_number(*(F*)x),
        r=janet_wrap_integer((int32_t)*(J*)x),
        r=janet_wrap_boolean(*(int*)x),
        nyi,
        r=janet_wrap_array(j_vf(*(U*)x)),
        r=janet_wrap_array(j_vi(*(U*)x)),
        r=janet_wrap_array(j_vb(*(U*)x)),
        nyi
    );
    R r;
}

Z Janet apple_call(U x, int32_t argc, Janet *argv) {
    JF *jit = (JF *)x;
    FnTy* ty=jit->ty;
    int aarg=ty->argc;
    janet_fixarity(argc, aarg);
    U* vals=janet_smalloc(sizeof(U)*argc);U ret=janet_smalloc(8);
    uint8_t fs=0;
    for(int k=0;k<aarg;k++){
        ArgTy(ty->args[k],
            {SA(F,xf);*xf=janet_getnumber(argv,k);vals[k]=xf;},
            {SA(J,xi);*xi=(J)janet_getinteger(argv,k);vals[k]=xi;},
            {SA(B,xb);*xb=(B)janet_getboolean(argv,k);vals[k]=xb;},
            nyi,
            {SA(U,a);*a=fv_r(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;},
            {SA(U,a);*a=fv_i(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;},
            {SA(U,a);*a=fv_b(janet_getarray(argv,k));fs|=1<<k;vals[k]=a;},
            nyi
        )
    }
    U fp=jit->bc;ffi_cif* cif=jit->ffi;
    ffi_call(cif,fp,ret,vals);
    Janet r=jr(ty->res,ret);
    iX(argc,$(fs>>i&1, free(*(U*)vals[i])))
    janet_sfree(vals);janet_sfree(ret);
    R r;
}

Z O jit_ts(U jit, JanetBuffer* buf) {JF* c=(JF*)jit;janet_buffer_push_cstring(buf,c->ts);}

G JanetAbstractType jit_t = {
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
    K char* inp=janet_getcstring(argv,0);
    T e;
        T o=apple_printty(inp,&e);
    ERR(o,e)
    R janet_cstringv(o);
}

Z Janet jit(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    janet_checktypes(argv[0], JANET_TFLAG_STRING);
    K char* inp=janet_getcstring(argv,0);
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

G JanetReg cfuns[] = {
    {"tyof", tyof_j, "type of expression"},
    {"jit", jit, "Compile source string into Janet callable"},
    {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
    hs_init(0,0);
    janet_cfuns(env, "apple", cfuns);
    janet_register_abstract_type(&jit_t);
}
