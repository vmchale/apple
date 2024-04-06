#include <stdlib.h>

#include"./include/apple_abi.h"

U poke_af (Af x) {
    I rnk = x.rnk;
    I t = 1;
    DO(i,rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+8*t);
    I* i_p = p;
    F* f_p = p;
    *i_p = rnk;
    DO(i,rnk,i_p[i+1]=x.dim[i]);
    DO(i,t,f_p[i+1+rnk]=x.xs[i]);
    R p;
}

U poke_ai (Ai x) {
    I rnk = x.rnk;
    I t = 1;
    DO(i,rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+8*t);
    I* i_p = p;
    *i_p = rnk;
    DO(i,rnk,i_p[i+1]=x.dim[i]);
    DO(i,t,i_p[i+1+rnk]=x.xs[i]);
    R p;
}
