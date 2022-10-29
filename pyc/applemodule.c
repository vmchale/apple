#include <HsFFI.h>
#include <Python.h>
#include <apple.h>
#include<numpy/arrayobject.h>
#include<apple_abi.h>

#define U void*
#define R return

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

typedef U (*Ufp)(void);
typedef I (*Ifp)(void);
typedef F (*Ffp)(void);

static PyObject* apple_apple(PyObject *self, PyObject *args) {
    const char* inp;
    PyArg_ParseTuple(args, "s", &inp);
    char* err;char** err_p = &err;
    enum apple_t ty=apple_ty(inp,err_p);
    if (ty == -1) {
        PyErr_SetString(PyExc_RuntimeError, err);
        free(err);R NULL;
    };
    U fp;
    if (ty != Fn){fp=apple_compile(inp);}
    switch(ty){
        case Fn: R PyUnicode_FromString("(function)");
        case IA: R npy_i(((Ufp) fp)());
        case FA: R npy_f(((Ufp) fp)());
        case F_t: R PyFloat_FromDouble(((Ffp) fp)());
        case I_t: R PyLong_FromLongLong(((Ifp) fp)());
    }
    // FIXME: function pointer is never freed
    Py_RETURN_NONE;
}

static PyMethodDef AppleMethods[] = {
    {"apple", apple_apple, METH_VARARGS, "JITed array"},
    {"typeof", apple_typeof, METH_VARARGS, "Display type of expression"},
    {NULL,NULL,0,NULL}
};

static struct PyModuleDef applemodule = { PyModuleDef_HEAD_INIT, "apple", NULL, -1, AppleMethods };

PyMODINIT_FUNC PyInit_apple(void) { hs_init(0,0); import_array(); R PyModule_Create(&applemodule); }
