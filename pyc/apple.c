#include <HsFFI.h>
#include <Python.h>
#include<sys/mman.h>
#include<numpy/arrayobject.h>
#include"../include/apple_abi.h"
#include"../c/ffi.c"

typedef PyObject* PY;typedef PyArrayObject* NP;

#define CT(o,c,s) {PyArray_Descr *d=PyArray_DESCR(o);if(!(d->type==c)){PyErr_SetString(PyExc_RuntimeError,s);}}
#define ERR(p,msg) {if(p==NULL){PyErr_SetString(PyExc_RuntimeError,msg);free(msg);R NULL;};}

#define ZF Z PY

TS CA {U h;U d;} CA;
Z void CA_dealloc(CA* self) {free(self->h);}

static PyTypeObject AT = {
    PyVarObject_HEAD_INIT(NULL,0)
    .tp_name="apple.ANumPy",
    .tp_basicsize = sizeof(CA),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = PyType_GenericNew,
    .tp_dealloc = (destructor)CA_dealloc,
};

// CD - copy dims AD - apple dimensionate
#define CD(rnk,x,t,ls) J* i_p=x;J rnk=i_p[0];npy_intp* ls=malloc(SZ(npy_intp)*rnk);J t=1;DO(i,rnk,t*=i_p[i+1];ls[i]=(npy_intp)i_p[i+1]);
#define AD(r,x,py) {J* x_i=x;x_i[0]=r;npy_intp* ls=PyArray_DIMS(py);DO(i,r,x_i[i+1]=(J)ls[i]);}
#define A(r,n,w,x,py) J r=PyArray_NDIM(py);J n=PyArray_SIZE(py);U x=malloc(8+8*r+n*w);AD(r,x,py)

// https://numpy.org/doc/stable/reference/c-api/array.html
ZU f_npy(const NP o) {CT(o,'d',"Error: expected an array of floats");A(rnk,n,8,x,o);F* x_f=x;U data=PyArray_DATA(o);memcpy(x_f+rnk+1,data,n*8);R x;}
ZU b_npy(const NP o) {CT(o,'?',"Error: expected an array of booleans");A(rnk,n,1,x,o);B* x_p=x;U data=PyArray_DATA(o);memcpy(x_p+8*rnk+8,data,n);R x;}
ZU i_npy(const NP o) {CT(o,'l',"Error: expected an array of 64-bit integers");A(rnk,n,8,x,o);J* x_i=x;U data=PyArray_DATA(o);memcpy(x_i+rnk+1,data,n*8);R x;}

#define RP(rnk,x,n,w,ls,T) {S sz=w*n;CA* d=PyObject_New(CA,&AT);d->h=x;d->d=x+rnk*8+8;PyArray_Descr* pd=PyArray_DescrFromType(T); PY r=PyArray_NewFromDescr(&PyArray_Type,pd,(int)rnk,ls,NULL,d->d,NPY_ARRAY_C_CONTIGUOUS,NULL); R r;}

#define NPA(f,s,T) ZF f(U x) {CD(rnk,x,t,ls);RP(rnk,x,t,s,ls,T);}

NPA(npy_i,8,NPY_INT64)
NPA(npy_f,8,NPY_FLOAT64)
NPA(npy_b,1,NPY_BOOL)

ZF apple_typeof(PY self, PY args) {
    const T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
        T res = apple_printty(inp,&err);
    ERR(res,err);
    PY py = PyUnicode_FromString(res);
    free(res);R py;
}

ZF apple_asm(PY self, PY args) {
    const T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
        T res = apple_dumpasm(inp,&err);
    ERR(res,err);
    PY py = PyUnicode_FromString(res);
    free(res);R py;
}

ZF apple_ir(PY self, PY args) {
    const T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
        T res = apple_dumpir(inp,&err);
    ERR(res,err);
    PY py = PyUnicode_FromString(res);
    free(res); R py;
}

TS JO {
    PyObject_HEAD
    U bc;S c_sz;FnTy* ty; U sa;ffi_cif* ffi;T ts;
} JO;

Z void cache_dealloc(JO* self) {
    munmap(self->bc,self->c_sz);
    free(self->sa);freety(self->ty);free(self->ffi);free(self->ts);
}

ZF apple_ts(JO* self) {R PyUnicode_FromString(self->ts);}

// file:///usr/share/doc/libffi8/html/The-Basics.html
ZF apple_call(PY self, PY args, PY kwargs) {
    JO* c=(JO*)self;PY arg0=NULL,arg1=NULL,arg2=NULL,arg3=NULL,arg4=NULL,arg5=NULL;
    PyArg_ParseTuple(args, "|OOOOOO", &arg0, &arg1, &arg2, &arg3, &arg4, &arg5);
    FnTy* ty=c->ty;U fp=c->bc;
    ffi_cif* cif=c->ffi;
    int argc=ty->argc;
    U* vals=alloca(SZ(U)*argc);U ret=alloca(8);
    PY pyarg, pyargs[]={arg0,arg1,arg2,arg3,arg4,arg5};
    uint8_t fs=0;
    for(int k=0;k<argc;k++){
        pyarg=pyargs[k];
        if(pyarg!=NULL){
            switch(ty->args[k]){
                C(IA,SA(U,x);*x=i_npy((NP)pyarg);fs|=1<<k;vals[k]=x;)
                C(BA,SA(U,x);*x=b_npy((NP)pyarg);fs|=1<<k;vals[k]=x;)
                C(FA,SA(U,x);*x=f_npy((NP)pyarg);fs|=1<<k;vals[k]=x;)
                C(I_t,SA(J,xi);*xi=PyLong_AsLong(pyarg);vals[k]=xi;)
                C(F_t,SA(F,xf);*xf=PyFloat_AsDouble(pyarg);vals[k]=xf;)
            }
        }
    }
    ffi_call(cif,fp,ret,vals);
    DO(i,argc,if(fs>>i&1){free(*(U*)vals[i]);})
    PY r;
    switch(ty->res){
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
    .tp_repr=(reprfunc)apple_ts,
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = PyType_GenericNew,
    .tp_call = apple_call,
    .tp_dealloc = (destructor)cache_dealloc,
};

ZF apple_jit(PY self, PY args) {
    const T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
    FnTy* ty=apple_ty(inp,&err);
    ERR(ty,err);
    S sz;
        T ts_str=apple_print_ts_sz(inp,&sz,&err);
    T buf=malloc(sz+8);
    sprintf(buf,"<fn : %s>",ts_str);free(ts_str);
    U fp,s;S f_sz;
    fp=apple_compile(&sys,inp,&f_sz,&s);
    JO* cc=PyObject_New(JO, &JOT);
    ffi_cif* ffi=apple_ffi(ty);
    cc->bc=fp;cc->c_sz=f_sz;cc->ty=ty;cc->sa=s;cc->ffi=ffi;cc->ts=buf;
    Py_INCREF(cc);
    R (PY)cc;
}

static PyMethodDef AFn[] = {
    {"jit", apple_jit, METH_VARARGS, "Compile an expression into a callable object"},
    {"typeof", apple_typeof, METH_VARARGS, "Display type of expression"},
    {"asm", apple_asm, METH_VARARGS, "Dump assembly"},
    {"ir", apple_ir, METH_VARARGS, "Dump IR (debug)"},
    {NULL,NULL,0,NULL}
};

Z struct PyModuleDef applemodule = { PyModuleDef_HEAD_INIT, "apple", NULL, -1, AFn };

PyMODINIT_FUNC PyInit_apple(void) { hs_init(0,0); import_array(); PY m=PyModule_Create(&applemodule); PyModule_AddType(m,&JOT); R m; }
