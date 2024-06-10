#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

#define pf printf
#define nl pf("\n")
#define pj(x) pf("%lld",x)
#define PA(s,n,x) DO(i,t,{pf(s,x[i]);if (i!=n-1){pf(",");}});nl

extern U aa(U);

void paf(U xs) {
    I* dims=xs;
    I rnk=dims[0];dims+=1;
    pj(rnk);pf(" ");
    I t=1;I d;
    DO(i,rnk,{d=dims[i];t*=d;pj(d);if (i!=rnk-1) {pf(",");}}) nl;
    F* e=xs+d+1;
    PA("%f",t,e);
}

int main(int argc, char *argv[]) {
    F xs[] = {0,1,1,0};
    I dx[] = {2,2};
    Af a = {2,dx,xs};
    U x = poke_af(a);
    paf(aa(x));
    free(x);
}
