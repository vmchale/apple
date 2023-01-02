#include <HsFFI.h>
#include <Python.h>
#include <apple.h>
#include<sys/mman.h>
#include<numpy/arrayobject.h>
#include<apple_abi.h>

#define U void*
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

typedef U (*Ufp)(void);
typedef I (*Ifp)(void);typedef F(*Ffp)(void);
typedef F (*Fffp)(F);typedef F(*Ifffp)(I,F);
typedef U (*Aafp)(U);
typedef F (*Affp)(U);typedef I (*Aifp)(U);
typedef F (*Aaffp)(U,U);
typedef U (*Iafp)(I);

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
};

static PyObject* apple_f(PyObject* self, PyObject* args) {
    PyCacheObject* c;PyObject* arg0;PyObject* arg1;PyObject* arg2; PyObject* arg3; PyObject* arg4;
    PyArg_ParseTuple(args, "O|OOOOO", &c, &arg0, &arg1, &arg2, &arg3, &arg4);
    FnTy* ty=c->ty;U fp=c->code;
    PyObject* r;
    Sw(ty->res){
        C IA:
            Sw(ty->argc){
                C 0: {r=npy_i(((Ufp) fp)());BR}
            }BR;
        C FA:
            Sw(ty->argc){
                C 0: {r=npy_f(((Ufp) fp)());BR}
                C 1: Sw(ty->args[0]){
                    C FA: {U inp0=f_npy(arg0);r=npy_f(((Aafp) fp)(inp0));BR};
                };BR
            };BR
        C F_t:
            Sw(ty->argc){
                C 0: r=PyFloat_FromDouble(((Ffp) fp)());BR
                C 1: Sw(ty->args[0]){
                    C FA: {U inp0=f_npy(arg0);r=PyFloat_FromDouble(((Affp) fp)(inp0));BR};
                    C F_t: {r=PyFloat_FromDouble(((Fffp) fp)(PyFloat_AsDouble(arg0)));BR};
                };BR
                C 2: Sw(ty->args[0]){
                    C FA: Sw(ty->args[1]){
                        C FA: {U inp0=f_npy(arg0);U inp1=f_npy(arg1);r=PyFloat_FromDouble(((Aaffp) fp)(inp0, inp1));BR};
                    };BR
                    C I_t: Sw(ty->args[1]){
                        C F_t: {I inp0=PyLong_AsLong(arg0);F inp1=PyFloat_AsDouble(arg1);r=PyFloat_FromDouble(((Ifffp) fp)(inp0,inp1));BR};
                    };BR
                };BR
            };BR
        C I_t:
            Sw(ty->argc){
                C 0: r=PyLong_FromLongLong(((Ifp) fp)());BR
                C 1: Sw(ty->args[0]){
                    C IA: {U inp0=i_npy(arg0);r=PyLong_FromLongLong(((Aifp) fp)(inp0));BR};
                };BR
            };BR
    }
    R r;
};

static PyObject* apple_apple(PyObject *self, PyObject *args) {
    const char* inp;PyObject* arg0;PyObject* arg1;PyObject* arg2; PyObject* arg3; PyObject* arg4; PyObject* arg5;
    PyArg_ParseTuple(args, "s|OOOOOO", &inp, &arg0, &arg1, &arg2, &arg3, &arg4, &arg5);
    char* err;char** err_p = &err;
    FnTy* ty=apple_ty(inp,err_p);
    if (ty == NULL) {
        PyErr_SetString(PyExc_RuntimeError, err);
        free(err);R NULL;
    };
    U fp;size_t f_sz;
    fp=apple_compile((P)&malloc,(P)&free,inp,&f_sz);
    PyObject* r;
    Sw(ty->res){
        C IA: r=npy_i(((Ufp) fp)());BR
        C FA:
            Sw(ty->argc){
                C 0: {r=npy_f(((Ufp) fp)());BR}
                C 1: Sw(ty->args[0]){C FA: {U inp0=f_npy(arg0);r=npy_f(((Aafp) fp)(inp0));BR};};BR
            };BR
        C F_t:
            Sw(ty->argc){
                C 0: r=PyFloat_FromDouble(((Ffp) fp)());BR
                C 1: Sw(ty->args[0]){C FA: {U inp0=f_npy(arg0);r=PyFloat_FromDouble(((Affp) fp)(inp0));BR}; C F_t: {r=PyFloat_FromDouble(((Fffp) fp)(PyFloat_AsDouble(arg0)));BR};};BR
                C 2: Sw(ty->args[0]){C FA: Sw(ty->args[1]){C FA: {U inp0=f_npy(arg0);U inp1=f_npy(arg1);r=PyFloat_FromDouble(((Aaffp) fp)(inp0, inp1));BR};};};BR
            };BR
        C I_t:
            Sw(ty->argc){
                C 0: r=PyLong_FromLongLong(((Ifp) fp)());BR
                C 1: Sw(ty->args[0]){C IA: {U inp0=i_npy(arg0);r=PyLong_FromLongLong(((Aifp) fp)(inp0));BR};};BR
            };BR
    }
    munmap(fp,f_sz);
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
