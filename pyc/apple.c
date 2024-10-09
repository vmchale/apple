#include <HsFFI.h>
#include <Python.h>
#include<sys/mman.h>
#include<numpy/arrayobject.h>
#include"../include/apple_abi.h"
#include"../c/ffi.c"

typedef void* U;typedef PyObject* PY;typedef PyArrayObject* NPA;typedef size_t S;

#define CT(o,c,s) {PyArray_Descr *d=PyArray_DESCR(o);if(!(d->type==c)){PyErr_SetString(PyExc_RuntimeError,s);}}
#define CD(t,rnk,x,dims) J* i_p=x;J rnk=i_p[0];npy_intp* dims=malloc(sizeof(npy_intp)*rnk);DO(i,rnk,t*=i_p[i+1];dims[i]=(npy_intp)i_p[i+1]);
#define A(r,sz,x,py) U x=malloc(sz);{J* x_i=x;x_i[0]=rnk;DO(i,rnk,x_i[i+1]=(J)py[i]);}
#define ERR(p,msg) {if(p==NULL){PyErr_SetString(PyExc_RuntimeError, msg);free(msg);R NULL;};}

// https://numpy.org/doc/stable/reference/c-api/array.html
U f_npy(const NPA o) {
    CT(o,'d',"Error: expected an array of floats")
    J rnk=PyArray_NDIM(o);
    npy_intp* dims=PyArray_DIMS(o);
    J n=PyArray_SIZE(o);
    A(rnk,(1+rnk+n)*8,x,dims);
    F* x_f=x;
    U data=PyArray_DATA(o);
    memcpy(x_f+rnk+1,data,n*8);
    R x;
}

U b_npy(NPA o) {
    CT(o,'?',"Error: expected an array of booleans")
    J rnk=PyArray_NDIM(o);
    npy_intp* dims=PyArray_DIMS(o);
    J n=PyArray_SIZE(o);
    A(rnk,(1+rnk)*8+n,x,dims);
    B* x_p=x;
    U data=PyArray_DATA(o);
    memcpy(x_p+8*rnk+8,data,n*8);
    R x;
}

U i_npy(NPA o) {
    CT(o,'l',"Error: expected an array of 64-bit integers")
    J rnk=PyArray_NDIM(o);
    npy_intp* dims=PyArray_DIMS(o);
    J n=PyArray_SIZE(o);
    A(rnk,(1+rnk+n)*8,x,dims);
    J* x_i=x;
    U data=PyArray_DATA(o);
    memcpy(x_i+rnk+1,data,n*8);
    R x;
}

PY npy_i(U x) {
    J t=1;
    CD(t,rnk,x,dims);
    S sz=8*t;
    U data=malloc(sz);
    memcpy(data,i_p+rnk+1,sz);
    PY res=PyArray_SimpleNewFromData(rnk,dims,NPY_INT64,data);
    PyArray_ENABLEFLAGS((NPA*)res,NPY_ARRAY_OWNDATA);
    free(x);R res;
}

PY npy_f(U x) {
    J t=1;
    CD(t,rnk,x,dims);
    S sz=8*t;
    U data=malloc(sz);
    memcpy(data,i_p+rnk+1,sz);
    PY res=PyArray_SimpleNewFromData(rnk,dims,NPY_FLOAT64,data);
    PyArray_ENABLEFLAGS((NPA*)res,NPY_ARRAY_OWNDATA);
    free(x);R res;
}

PY npy_b(U x) {
    S t=1;
    CD(t,rnk,x,dims);
    U data=malloc(t);
    B* x_p=x;
    memcpy(data,x_p+rnk*8+8,t);
    PY res=PyArray_SimpleNewFromData(rnk,dims,NPY_BOOL,data);
    PyArray_ENABLEFLAGS((NPA*)res,NPY_ARRAY_OWNDATA);
    free(x);R res;
}

void freety(FnTy* x){free(x->args);free(x);}

static PY apple_typeof(PY self, PY args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p = &err;
    char* res = apple_printty(inp,err_p);
    ERR(res,err);
    PY py = PyUnicode_FromString(res);
    free(res);R py;
}

static PY apple_asm(PY self, PY args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p = &err;
    char* res = apple_dumpasm(inp,err_p);
    ERR(res,err);
    PY py = PyUnicode_FromString(res);
    free(res);R py;
}

static PY apple_ir(PY self, PY args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p = &err;
    char* res = apple_dumpir(inp,err_p);
    ERR(res,err);
    PY py = PyUnicode_FromString(res);
    free(res); R py;
}

typedef struct JO {
    PyObject_HEAD
    U bc;S c_sz;FnTy* ty; U sa;ffi_cif* ffi;
} JO;

static void cache_dealloc(JO* self) {
    munmap(self->bc,self->c_sz);
    free(self->sa);freety(self->ty);free(self->ffi);
}

// file:///usr/share/doc/libffi8/html/The-Basics.html
static PY apple_call(PY self, PY args, PY kwargs) {
    JO* c=self;PY arg0=NULL;PY arg1=NULL;PY arg2=NULL;PY arg3=NULL;PY arg4=NULL;PY arg5=NULL;
    PyArg_ParseTuple(args, "|OOOOOO", &arg0, &arg1, &arg2, &arg3, &arg4, &arg5);
    FnTy* ty=c->ty;U fp=c->bc;
    PY r;
    ffi_cif* cif=c->ffi;
    int argc=ty->argc;
    U* vals=alloca(sizeof(U)*argc);U ret=alloca(8);
    PY pyarg;PY pyargs[]={arg0,arg1,arg2,arg3,arg4,arg5};
    uint8_t fs=0;
    for(int k=0;k<argc;k++){
        pyarg=pyargs[k];
        if(pyarg!=NULL){
            Sw(ty->args[k]){
                C(IA,U* x=alloca(sizeof(U));x[0]=i_npy((NPA)pyarg);fs|=1<<k;vals[k]=x;)
                C(BA,U* x=alloca(sizeof(U));x[0]=b_npy((NPA)pyarg);fs|=1<<k;vals[k]=x;)
                C(FA,U* x=alloca(sizeof(U));x[0]=f_npy((NPA)pyarg);fs|=1<<k;vals[k]=x;)
                C(I_t,J* xi=alloca(sizeof(J));xi[0]=PyLong_AsLong(pyarg);vals[k]=xi;)
                C(F_t,F* xf=alloca(sizeof(F));xf[0]=PyFloat_AsDouble(pyarg);vals[k]=xf;)
            }
        }
    }
    ffi_call(cif,fp,ret,vals);
    DO(i,argc,if(fs>>i&1){free(*(U*)vals[i]);})
    Sw(ty->res){
        C(IA,r=npy_i(*(U*)ret))
        C(FA,r=npy_f(*(U*)ret))
        C(BA,r=npy_b(*(U*)ret))
        C(F_t,r=PyFloat_FromDouble(*(F*)ret))
        C(I_t,r=PyLong_FromLongLong(*(J*)ret))
        C(B_t,r=PyBool_FromLong(*(long*)ret))
    }
    R r;
};

static PyTypeObject JOT = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "apple.AppleJIT",
    .tp_doc = PyDoc_STR("JIT-compiled function in-memory"),
    .tp_basicsize = sizeof(JO),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = PyType_GenericNew,
    .tp_call = apple_call,
    .tp_dealloc = (destructor)cache_dealloc,
};

static PY apple_jit(PY self, PY args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p=&err;
    FnTy* ty=apple_ty(inp,err_p);
    ERR(ty,err);
    U fp;S f_sz;U s;
    fp=apple_compile(&sys,inp,&f_sz,&s);
    JO* cc=PyObject_New(JO, &JOT);
    ffi_cif* ffi=apple_ffi(ty);
    cc->bc=fp;cc->c_sz=f_sz;cc->ty=ty;cc->sa=s;cc->ffi=ffi;
    Py_INCREF(cc);
    R (PY)cc;
}

static PyMethodDef AFn[] = {
    {"jit", apple_jit, METH_VARARGS, "Compile an expressoin into a callable object"},
    {"typeof", apple_typeof, METH_VARARGS, "Display type of expression"},
    {"asm", apple_asm, METH_VARARGS, "Dump assembly"},
    {"ir", apple_ir, METH_VARARGS, "Dump IR (debug)"},
    {NULL,NULL,0,NULL}
};

static struct PyModuleDef applemodule = { PyModuleDef_HEAD_INIT, "apple", NULL, -1, AFn };

PyMODINIT_FUNC PyInit_apple(void) { hs_init(0,0); import_array(); PY m=PyModule_Create(&applemodule); PyModule_AddType(m,&JOT); R m; }
