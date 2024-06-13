#include"./apple_abi.h"

#define pf printf
#define nl pf("\n")
#define pj(x) pf("%lld",x)
#define PA(s,n,x) DO(i,t,{pf(s,x[i]);if (i!=n-1){pf(",");}});nl

void paf(U xs) {
    I* dims=xs;
    I rnk=dims[0];dims+=1;
    pj(rnk);pf(" ");
    I t=1;I d;
    DO(i,rnk,{d=dims[i];t*=d;pj(d);if (i!=rnk-1) {pf(",");}}) nl;
    F* e=xs+rnk+1;
    PA("%f",t,e);
}
