#include <stdlib.h>
#include <stdbool.h>
#include"m.h"

#define V(n,xs,p) U p;{p=malloc(16+8*n);J* i_p=p;*i_p=1;i_p[1]=n;memcpy(p+16,xs,8*n);}

// for https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170#return-values
TS F2{ F x, y; } F2;

TS Af { J rnk, *dim; F* xs; } Af;
TS Ai { J rnk, *dim; J* xs; } Ai;
TS Ab { J rnk, *dim; B* xs; } Ab;

_ U poke_af (Af x) {
    J rnk = x.rnk;
    J t = 1;
    iX(rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+8*t);
    J* i_p = p;F* f_p = p;
    i_p[0] = rnk;
    iX(rnk,i_p[i+1]=x.dim[i]);
    iX(t,f_p[i+1+rnk]=x.xs[i]);
    R p;
}

_ U poke_ai (Ai x) {
    J rnk = x.rnk;
    J t = 1;
    iX(rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+8*t);
    J* i_p = p;
    i_p[0] = rnk;
    iX(rnk,i_p[i+1]=x.dim[i]);
    iX(t,i_p[i+1+rnk]=x.xs[i]);
    R p;
}

_ U poke_ab (Ab x) {
    J rnk = x.rnk;
    J t = 1;
    iX(rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+t);
    J* i_p = p;B* b_p=p;
    i_p[0] = rnk;
    iX(rnk,i_p[i+1]=x.dim[i]);
    b_p+=8+8*rnk;
    iX(t,b_p[i]=x.xs[i]);
    R p;
}
