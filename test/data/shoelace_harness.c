#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define R return
#define DO(i,n,a) {I i;for(i=0;i<n;i++){a;}}

typedef double F;typedef int64_t I; typedef void* U;

typedef struct Af {I rnk; I* dim; F* xs;} Af;

U poke_af (Af x) {
    I rnk = x.rnk;
    I t = 1;
    DO(i,rnk,t*=x.dim[i]);
    U p = malloc(8+8*x.rnk+8*t);
    I* i_p = p;F* f_p = p;
    *i_p = rnk;
    DO(i,rnk,i_p[i+1]=x.dim[i]);
    DO(i,t,f_p[i+1+rnk]=x.xs[i]);
    R p;
}

extern F shoelace(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {0,4,4};
    F ys[] = {0,0,3};
    I d[] = {3};
    Af a = {1,d,xs};
    Af b = {1,d,ys};
    U x = poke_af(a);U y = poke_af(b);
    printf("%f\n", shoelace(x,y));
    free(x);free(y);
}
