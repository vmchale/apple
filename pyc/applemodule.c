#include <HsFFI.h>
#include <Python.h>
#include<sys/mman.h>
#include<numpy/arrayobject.h>
#include"../include/apple_abi.h"
#include"../c/ffi.c"

typedef void* U;typedef PyObject* PO;typedef size_t S;

#define R return
#define Sw switch
#define C case
#define BR break;
#define CT(o,c,s) {PyArray_Descr *d=PyArray_DESCR(o);if(!(d->type==c)){PyErr_SetString(PyExc_RuntimeError, s);}}

// https://numpy.org/doc/stable/reference/c-api/array.html
U f_npy(PyObject* o) {
    I rnk=PyArray_NDIM(o);
    npy_intp* dims=PyArray_DIMS(o);
    I n=PyArray_SIZE(o);
    I sz_i=1+rnk+n;
    CT(o,'d',"Error: expected an array of floats")
    S sz=sz_i*8;
    U x=malloc(sz);I* x_i=x; F* x_f=x;
    x_i[0]=rnk;
    DO(i,rnk,x_i[i+1]=(I)dims[i]);
    U data=PyArray_DATA(o);
    memcpy(x_f+rnk+1,data,n*8);
    R x;
}

U i_npy(PyObject* o) {
    I rnk=PyArray_NDIM(o);
    npy_intp* dims=PyArray_DIMS(o);
    I n=PyArray_SIZE(o);
    I sz_i=1+rnk+n;
    S sz=sz_i*8;
    CT(o,'l',"Error: expected an array of 64-bit integers")
    U x=malloc(sz);I* x_i=x;
    x_i[0]=rnk;
    DO(i,rnk,x_i[i+1]=(I)dims[i]);
    U data=PyArray_DATA(o);
    memcpy(x_i+rnk+1,data,n*8);
    R x;
}

PyObject* npy_i(U x) {
    I* i_p=x;
    I t=1;
    I rnk=i_p[0];
    long* dims=malloc(sizeof(long)*rnk);
    DO(i,rnk,t*=i_p[i+1];dims[i]=(long)i_p[i+1]);
    S sz=8*t;
    U data=malloc(sz);
    memcpy(data,i_p+rnk+1,sz);
    PyObject* res=PyArray_SimpleNewFromData(rnk,dims,NPY_INT64,data);
    PyArray_ENABLEFLAGS((PyArrayObject*)res,NPY_ARRAY_OWNDATA);
    free(x);R res;
}

PyObject* npy_f(U x) {
    I* i_p=x;
    I t=1;
    I rnk=i_p[0];
    long* dims=malloc(sizeof(long)*rnk);
    DO(i,rnk,t*=i_p[i+1];dims[i]=(long)i_p[i+1]);
    S sz=8*t;
    U data=malloc(sz);
    memcpy(data,i_p+rnk+1,sz);
    PyObject* res=PyArray_SimpleNewFromData(rnk,dims,NPY_FLOAT64,data);
    PyArray_ENABLEFLAGS((PyArrayObject*)res,NPY_ARRAY_OWNDATA);
    free(x);R res;
}

void freety(FnTy* x){free(x->args);free(x);}

static PyObject* apple_typeof(PyObject* self, PyObject *args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p = &err;
    char* res = apple_printty(inp,err_p);
    if (res == NULL) {
        PyErr_SetString(PyExc_RuntimeError, err);
        free(err);R NULL;
    }
    PyObject* pyres = PyUnicode_FromString(res);
    free(res);
    R pyres;
}

static PyObject* apple_asm(PyObject* self, PyObject *args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p = &err;
    char* res = apple_dumpasm(inp,err_p);
    if (res == NULL) {
        PyErr_SetString(PyExc_RuntimeError, err);
        free(err);R NULL;
    }
    PyObject* pyres = PyUnicode_FromString(res);
    free(res);
    R pyres;
}

static PyObject* apple_ir(PyObject* self, PyObject *args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p = &err;
    char* res = apple_dumpir(inp,err_p);
    if (res == NULL) {
        PyErr_SetString(PyExc_RuntimeError, err);
        free(err);R NULL;
    }
    PyObject* pyres = PyUnicode_FromString(res);
    free(res);
    R pyres;
}

typedef struct PyCacheObject {
    PyObject_HEAD
    U code;S code_sz;FnTy* ty; U sa;ffi_cif* ffi;
} PyCacheObject;

static void cache_dealloc(PyCacheObject* self) {
    munmap(self->code,self->code_sz);
    free(self->sa);free(self->ty);free(self->ffi);
}

static PyTypeObject CacheType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "Cache",
    .tp_doc = PyDoc_STR("Cached JIT function"),
    .tp_basicsize = sizeof(PyCacheObject),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = PyType_GenericNew,
    .tp_dealloc = &cache_dealloc,
};

static PyObject* apple_cache(PyObject *self, PyObject *args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p=&err;
    FnTy* ty=apple_ty(inp,err_p);
    if(ty == NULL){
        PyErr_SetString(PyExc_RuntimeError, err);
        free(err);R NULL;
    };
    U fp;S f_sz;U s;
    fp=apple_compile((P)&malloc,(P)&free,inp,&f_sz,&s);
    PyCacheObject* cc=PyObject_New(PyCacheObject, &CacheType);
    ffi_cif* ffi=apple_ffi(ty);
    cc->code=fp;cc->code_sz=f_sz;cc->ty=ty;cc->sa=s;cc->ffi=ffi;
    Py_INCREF(cc);
    R (PyObject*)cc;
}

static PyObject* apple_f(PyObject* self, PyObject* args) {
    PyCacheObject* c;PO arg0=NULL;PO arg1=NULL;PO arg2=NULL;PO arg3=NULL;PO arg4=NULL;PO arg5=NULL;
    PyArg_ParseTuple(args, "O|OOOOOO", &c, &arg0, &arg1, &arg2, &arg3, &arg4, &arg5);
    FnTy* ty=c->ty;U fp=c->code;
    PO r;
    ffi_cif* cif=c->ffi;
    int argc=ty->argc;
    U* vals=alloca(sizeof(U)*argc);
    U ret=alloca(8);
    PO pyarg;PO pyargs[]={arg0,arg1,arg2,arg3,arg4,arg5};
    for(int k=0;k<argc;k++){
        pyarg=pyargs[k];
        if(pyarg!=NULL){
            Sw(ty->args[k]){
                C IA: {U* x=alloca(sizeof(U));x[0]=i_npy(pyarg);vals[k]=x;};BR
                C FA: {U* x=alloca(sizeof(U));x[0]=f_npy(pyarg);vals[k]=x;};BR
                C I_t: {I* xi=alloca(sizeof(I));xi[0]=PyLong_AsLong(pyarg);vals[k]=xi;};BR
                C F_t: {F* xf=alloca(sizeof(F));xf[0]=PyFloat_AsDouble(pyarg);vals[k]=xf;};BR
            }
        }
    }
    ffi_call(cif,fp,ret,vals);
    Sw(ty->res){
        C IA: r=npy_i(*(U*)ret);BR
        C FA: r=npy_f(*(U*)ret);BR
        C F_t: r=PyFloat_FromDouble(*(F*)ret);BR
        C I_t: r=PyLong_FromLongLong(*(I*)ret);BR
    }
    R r;
};


// file:///usr/share/doc/libffi8/html/The-Basics.html
static PyObject* apple_apple(PyObject *self, PyObject *args) {
    const char* inp;PO arg0=NULL;PO arg1=NULL;PO arg2=NULL;PO arg3=NULL;PO arg4=NULL;PO arg5=NULL;
    PyArg_ParseTuple(args, "s|OOOOOO", &inp, &arg0, &arg1, &arg2, &arg3, &arg4, &arg5);
    char* err;char** err_p = &err;
    FnTy* ty=apple_ty(inp,err_p);
    if (ty == NULL) {
        PyErr_SetString(PyExc_RuntimeError, err);
        free(err);R NULL;
    };
    U fp;S f_sz;U s;
    fp=apple_compile((P)&malloc,(P)&free,inp,&f_sz,&s);
    PO r;
    ffi_cif* cif=apple_ffi(ty);
    int argc=ty->argc;
    U* vals=malloc(sizeof(U)*argc);
    U ret=malloc(8);
    PO pyarg;PO pyargs[]={arg0,arg1,arg2,arg3,arg4,arg5};
    // FIXME: fails for 2 float args
    for(int k=0;k<argc;k++){
        pyarg=pyargs[k];
        if(pyarg!=NULL){
            Sw(ty->args[k]){
                C IA: {U* x=malloc(sizeof(U));x[0]=i_npy(pyarg);vals[k]=x;};BR
                C FA: {U* x=malloc(sizeof(U));x[0]=f_npy(pyarg);vals[k]=x;};BR
                C I_t: {I* xi=malloc(sizeof(I));xi[0]=PyLong_AsLong(pyarg);vals[k]=xi;};BR
                C F_t: {F* xf=malloc(sizeof(F));xf[0]=PyFloat_AsDouble(pyarg);vals[k]=xf;};BR
            }
        }
    }
    ffi_call(cif,fp,ret,vals);
    DO(k,argc,free(vals[k]));
    free(vals);free(cif);freety(ty);
    munmap(fp,f_sz);
    Sw(ty->res){
        C IA: r=npy_i(*(U*)ret);BR
        C FA: r=npy_f(*(U*)ret);BR
        C F_t: r=PyFloat_FromDouble(*(F*)ret);BR
        C I_t: r=PyLong_FromLongLong(*(I*)ret);BR
    }
    // TODO: free other parts of cif (args)
    free(ret);free(s);
    R r;
}

static PyMethodDef AppleMethods[] = {
    {"f", apple_f, METH_VARARGS, "Run a cached function"},
    {"cache", apple_cache, METH_VARARGS, "JIT a function"},
    {"apple", apple_apple, METH_VARARGS, "JITed array"},
    {"typeof", apple_typeof, METH_VARARGS, "Display type of expression"},
    {"asm", apple_asm, METH_VARARGS, "Dump x86 assembly"},
    {"ir", apple_ir, METH_VARARGS, "Dump IR (debug)"},
    {NULL,NULL,0,NULL}
};

static struct PyModuleDef applemodule = { PyModuleDef_HEAD_INIT, "apple", NULL, -1, AppleMethods };

PyMODINIT_FUNC PyInit_apple(void) { hs_init(0,0); import_array(); R PyModule_Create(&applemodule); }
