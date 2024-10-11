#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define R return

#undef I

typedef double F;typedef int64_t J; typedef void* U; typedef uint8_t B;

#define DO(i,n,a) {J i;for(i=0;i<n;i++){a;}}
#define V(n,xs,p) {p=malloc(16+8*n);J* i_p=p;*i_p=1;i_p[1]=n;memcpy(p+16,xs,8*n);}

typedef struct Af { J rnk; J* dim; F* xs; } Af;
typedef struct Ai { J rnk; J* dim; J* xs; } Ai;
typedef struct Ab { J rnk; J* dim; B* xs; } Ab;

static U poke_af (Af x) {
    J rnk = x.rnk;
    J t = 1;
    DO(i,rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+8*t);
    J* i_p = p;F* f_p = p;
    i_p[0] = rnk;
    DO(i,rnk,i_p[i+1]=x.dim[i]);
    DO(i,t,f_p[i+1+rnk]=x.xs[i]);
    R p;
}

static U poke_ai (Ai x) {
    J rnk = x.rnk;
    J t = 1;
    DO(i,rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+8*t);
    J* i_p = p;
    i_p[0] = rnk;
    DO(i,rnk,i_p[i+1]=x.dim[i]);
    DO(i,t,i_p[i+1+rnk]=x.xs[i]);
    R p;
}
