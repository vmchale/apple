#include <HsFFI.h>
#include <Python.h>
#include<sys/mman.h>
#include<numpy/arrayobject.h>
#include"../include/apple_abi.h"
#include"../c/ffi.c"

typedef PyObject* PY;typedef PyArrayObject* NP;typedef const PY PYA;

#define $e(p,e) if(!(p)){PyErr_SetString(PyExc_RuntimeError,e);R NULL;}
#define $arr(o) $e(PyArray_CheckExact(o),"Expected NumPy array.")
#define CT(o,c,s) {$e((o->flags && NPY_ARRAY_C_CONTIGUOUS),"Only row-major (C-style) arrays are supported.");PyArray_Descr *d=PyArray_DESCR(o);$e((d->type==c),s);}
#define PyE(p,msg) if(p==NULL){PyErr_SetString(PyExc_RuntimeError,msg);free(msg);R NULL;}

#define ZF Z PY

_ O c_free(PY cap){free(PyCapsule_GetPointer(cap,NULL));}

// CD - copy dims AD - apple dimensionate
#define CD(rnk,x,ls) J* i_p=x;J rnk=i_p[0];npy_intp* ls=malloc(SZ(npy_intp)*rnk);DO(i,rnk,ls[i]=(npy_intp)i_p[i+1]);
#define AD(r,x,py) {J* x_i=x;x_i[0]=r;npy_intp* ls=PyArray_DIMS(py);DO(i,r,x_i[i+1]=(J)ls[i]);}
#define A(r,n,w,x,py) J r=PyArray_NDIM(py);J n=PyArray_SIZE(py);U x=malloc(8+8*r+n*w);AD(r,x,py)

// TODO: __func__
#define nyi {PyErr_SetString(PyExc_RuntimeError,"unsupported: tuple arguments in bindings.");R NULL;}

// https://numpy.org/doc/stable/reference/c-api/array.html
ZU f_npy(K NP o) {CT(o,'d',"Error: expected an array of floats");A(rnk,n,8,x,o);F* x_f=x;U data=PyArray_DATA(o);memcpy(x_f+rnk+1,data,n*8);R x;}
ZU b_npy(K NP o) {CT(o,'?',"Error: expected an array of booleans");A(rnk,n,1,x,o);B* x_p=x;U data=PyArray_DATA(o);memcpy(x_p+8*rnk+8,data,n);R x;}
ZU i_npy(K NP o) {CT(o,'l',"Error: expected an array of 64-bit integers");A(rnk,n,8,x,o);J* x_i=x;U data=PyArray_DATA(o);memcpy(x_i+rnk+1,data,n*8);R x;}

// CA - "encapsulate Apple" https://stackoverflow.com/a/52737023/11296354
#define CA(rnk,x,ls,d) {PY cap=PyCapsule_New(x,NULL,c_free);PY r=PyArray_NewFromDescr(&PyArray_Type,d,(int)rnk,ls,NULL,x+rnk*8+8,NPY_ARRAY_C_CONTIGUOUS,NULL);PyArray_SetBaseObject((NP)r,cap);R r;}
#define RP(rnk,x,ls,T) {PyArray_Descr* pd=PyArray_DescrFromType(T);CA(rnk,x,ls,pd)}

#define NPA(f,s,T) _ PY f(U x) {CD(rnk,x,ls);RP(rnk,x,ls,T);}

NPA(npy_i,8,NPY_INT64)
NPA(npy_f,8,NPY_FLOAT64)
NPA(npy_b,1,NPY_BOOL)

Z PY apy(K apple_t,K U);

// https://numpy.org/devdocs/reference/arrays.dtypes.html#specifying-and-constructing-data-types
_ PY npy_p(K apple_P t, U x){
    PyArray_Descr* pd;
    {int n=t.pi_n;T s=alloca(3*n+1);J l;J o=0;DO(i,n,$e(t.a_pi[i].f==Rc,"tuples-of-tuples not yet implemented.");T r;switch(t.a_pi[i].ty.aa){C(F_t,r="f8,";l=3) C(B_t,r="?,";l=2) C(I_t,r="i8,";l=3)};memcpy(s+o,r,l);o+=l);s[o]=0;PyArray_DescrConverter(PyUnicode_FromString(s), &pd);}
    CD(rnk,x,ls);
    CA(rnk,x,ls,pd);
}

_ PY ar(K apple_P t, K U* x){
    int n=t.pi_n;
    PY r=PyTuple_New(n);
    SA(U,ret);DO(i,n,ret[0]=x[i];PyTuple_SetItem(r,i,apy(t.a_pi[i],ret)))
    // libffi passes pointers as pointers-to-pointers
    R r;
}

// https://stackoverflow.com/a/66248758/11296354

Z PY apy(K apple_t t, K U x){
    PY r;
    ArgTy(t,
        r=PyFloat_FromDouble(*(F*)x),r=PyLong_FromLongLong(*(J*)x),r=PyBool_FromLong(*(long*)x),
        r=ar(t.ty.APi,*(U*)x),
        r=npy_f(*(U*)x),r=npy_i(*(U*)x),r=npy_b(*(U*)x),r=npy_p(t.ty.APi,*(U*)x)
    )
    R r;
}

ZF apple_typeof(PYA self, PYA args) {
    T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
        T res = apple_printty(inp,&err);
    PyE(res,err);
    K PY py = PyUnicode_FromString(res);
    free(res);R py;
}

ZF apple_asm(PYA self, PYA args) {
    T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
        T res = apple_dumpasm(inp,&err);
    PyE(res,err);
    K PY py = PyUnicode_FromString(res);
    free(res);R py;
}

ZF apple_ir(PYA self, PYA args) {
    T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
        T res = apple_dumpir(inp,&err);
    PyE(res,err);
    K PY py = PyUnicode_FromString(res);
    free(res); R py;
}

TS JO {
    PyObject_HEAD
    U bc;S c_sz;FnTy* ty; U sa;ffi_cif* ffi;T ts;
} JO;

_ O cache_dealloc(JO* self) {
    munmap(self->bc,self->c_sz);
    free(self->sa);freety(self->ty);free(self->ffi);free(self->ts);
}

ZF apple_ts(JO* self) {R PyUnicode_FromString(self->ts);}

// file:///usr/share/doc/libffi8/html/The-Basics.html
ZF apple_call(PYA self, PYA args, PYA kwargs) {
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
            apple_t argt=ty->args[k];
            switch(argt.f){
                    C(Rc,
                        switch(argt.ty.aa){
                            C(I_t,SA(J,xi);*xi=PyLong_AsLong(pyarg);vals[k]=xi;)
                            C(F_t,SA(F,xf);*xf=PyFloat_AsDouble(pyarg);vals[k]=xf;)
                        })
                    C(Pi,nyi)
                    C(Aa,
                        switch(argt.ty.aa){
                            C(I_t,SA(U,x);$arr(pyarg);*x=i_npy((const NP)pyarg);fs|=1<<k;vals[k]=x;)
                            C(B_t,SA(U,x);$arr(pyarg);*x=b_npy((const NP)pyarg);fs|=1<<k;vals[k]=x;)
                            C(F_t,SA(U,x);$arr(pyarg);*x=f_npy((const NP)pyarg);fs|=1<<k;vals[k]=x;)
                        })
                    C(Ap,nyi)
            }
        }
    }
    ffi_call(cif,fp,ret,vals);
    DO(i,argc,$(fs>>i&1,free(*(U*)vals[i])))
    R apy(ty->res,ret);
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

ZF apple_jit(PYA self, PYA args) {
    T inp;PyArg_ParseTuple(args, "s", &inp);
    T err;
    FnTy* ty=apple_ty(inp,&err);
    PyE(ty,err);
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
