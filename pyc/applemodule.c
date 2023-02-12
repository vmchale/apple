#include <HsFFI.h>
#include <Python.h>
#include<sys/mman.h>
#include<numpy/arrayobject.h>
#include"../c/jit.h"
#include"../c/ffi.c"

typedef void* U;typedef PyObject* PO;

#define R return
#define Sw switch
#define C case
#define BR break;

// https://numpy.org/doc/stable/reference/c-api/array.html
U f_npy(PyObject* o) {
    I rnk=PyArray_NDIM(o);
    npy_intp* dims=PyArray_DIMS(o);
    I n=PyArray_SIZE(o);
    I sz_i=1+rnk+n;
    // FIXME: error when np.dtype /= float
    /* PyErr_SetString(PyExc_RuntimeError, "Expected array of floats"); */
    size_t sz=sz_i*8;
    U x=malloc(sz);I* x_i=x; F* x_f=x;
    x_i[0]=rnk;
    DO(i,rnk,x_i[i+1]=(I)dims[i]);
    U data=PyArray_DATA(o);
    memcpy(x_f+rnk+1,data,sz);
    R x;
}

U i_npy(PyObject* o) {
    I rnk=PyArray_NDIM(o);
    npy_intp* dims=PyArray_DIMS(o);
    I n=PyArray_SIZE(o);
    I sz_i=1+rnk+n;
    size_t sz=sz_i*8;
    U x=malloc(sz);I* x_i=x;
    x_i[0]=rnk;
    DO(i,rnk,x_i[i+1]=(I)dims[i]);
    U data=PyArray_DATA(o);
    memcpy(x_i+rnk+1,data,sz);
    R x;
}

PyObject* npy_i(U x) {
    I* i_p = x;
    I t = 1;
    I rnk = i_p[0];
    long* dims=malloc(sizeof(long)*rnk);
    DO(i,rnk,t*=i_p[i+1];dims[i]=(long)i_p[i+1]);
    size_t sz=8*t;
    U data=malloc(sz);
    memcpy(data,i_p+rnk+1,sz);
    PyObject* res=PyArray_SimpleNewFromData(rnk,dims,NPY_INT64,data);
    free(x);R res;
}

PyObject* npy_f(U x) {
    I* i_p = x;
    I t = 1;
    I rnk = i_p[0];
    long* dims=malloc(sizeof(long)*rnk);
    DO(i,rnk,t*=i_p[i+1];dims[i]=(long)i_p[i+1]);
    size_t sz=8*t;
    U data=malloc(sz);
    memcpy(data,i_p+rnk+1,sz);
    PyObject* res=PyArray_SimpleNewFromData(rnk,dims,NPY_FLOAT64,data);
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
    U code;size_t code_sz;FnTy* ty;
} PyCacheObject;

static PyTypeObject CacheType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "Cache",
    .tp_doc = PyDoc_STR("Cached JIT function"),
    .tp_basicsize = sizeof(PyCacheObject),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = PyType_GenericNew,
    // TODO: leaks memory lol
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
    U fp;size_t f_sz;
    fp=apple_compile((P)&malloc,(P)&free,inp,&f_sz);
    PyCacheObject* cc=PyObject_New(PyCacheObject, &CacheType);
    cc->code=fp;cc->code_sz=f_sz;cc->ty=ty;
    Py_INCREF(cc);
    R (PyObject*)cc;
}

static PyObject* apple_f(PyObject* self, PyObject* args) {
    PyCacheObject* c;PO arg0=NULL;PO arg1=NULL;PO arg2=NULL;PO arg3=NULL;PO arg4=NULL;PO arg5=NULL;
    PyArg_ParseTuple(args, "O|OOOOO", &c, &arg0, &arg1, &arg2, &arg3, &arg4, &arg5);
    FnTy* ty=c->ty;U fp=c->code;
    PO r;
    ffi_cif* cif=apple_ffi(ty);
    int argc=ty->argc;
    void** vals=malloc(sizeof(void*)*argc);
    U ret= malloc(sizeof(F));
    U x;I xi;F xf;
    PO pyarg;PO pyargs[]={arg0,arg1,arg2,arg3,arg4,arg5};
    for(int k=0;k<6;k++){
        pyarg=pyargs[k];
        if(argc>k && pyarg!=NULL){
            Sw(ty->args[k]){
                C IA: x=i_npy(pyarg);vals[k]=&x;BR
                C FA: x=f_npy(pyarg);vals[k]=&x;BR
                C I_t: xi=PyLong_AsLong(pyarg);vals[k]=&xi;BR
                C F_t: xf=PyFloat_AsDouble(pyarg);vals[k]=&xf;BR
            }
        }
    }
    ffi_call(cif,fp,ret,vals);
    free(vals);free(cif);
    Sw(ty->res){
        C IA: r=npy_i(*(void**)ret);BR
        C FA: r=npy_f(*(void**)ret);BR
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
    U fp;size_t f_sz;
    fp=apple_compile((P)&malloc,(P)&free,inp,&f_sz);
    PO r;
    ffi_cif* cif=apple_ffi(ty);
    int argc=ty->argc;
    void** vals=malloc(sizeof(U)*argc);
    U ret= malloc(sizeof(F));
    U x;I xi;F xf;
    PO pyarg;PO pyargs[]={arg0,arg1,arg2,arg3,arg4,arg5};
    for(int k=0;k<6;k++){
        pyarg=pyargs[k];
        if(argc>k && pyarg!=NULL){
            Sw(ty->args[k]){
                C IA: x=i_npy(pyarg);vals[k]=&x;BR
                C FA: x=f_npy(pyarg);vals[k]=&x;BR
                C I_t: xi=PyLong_AsLong(pyarg);vals[k]=&xi;BR
                C F_t: xf=PyFloat_AsDouble(pyarg);vals[k]=&xf;BR
            }
        }
    }
    ffi_call(cif,fp,ret,vals);
    free(vals);free(cif);freety(ty);
    munmap(fp,f_sz);
    Sw(ty->res){
        C IA: r=npy_i(*(void**)ret);BR
        C FA: r=npy_f(*(void**)ret);BR
        C F_t: r=PyFloat_FromDouble(*(F*)ret);BR
        C I_t: r=PyLong_FromLongLong(*(I*)ret);BR
    }
    // TODO: free other parts of cif (args)
    free(ret);
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
