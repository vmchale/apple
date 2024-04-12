#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define R return

typedef double F;typedef int64_t I; typedef void* U; typedef bool B;

#define DO(i,n,a) {I i;for(i=0;i<n;i++){a;}}
#define V(n,xs,p) {p=malloc(16+8*n);I* i_p=p;*i_p=1;i_p[1]=n;memcpy(p+16,xs,8*n);}

typedef struct Af { I rnk; I* dim; F* xs; } Af;
typedef struct Ai { I rnk; I* dim; I* xs; } Ai;

static U poke_af (Af x) {
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

static U poke_ai (Ai x) {
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
