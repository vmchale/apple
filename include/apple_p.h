#include"./apple_abi.h"

#define pf printf
#define nl pf("\n")
#define pj(x) pf("%lld",x)
#define PA(s,n,x) DO(i,t,{pf(s,x[i]);if (i!=n-1){pf(",");}});nl

void paf(U xs) {
    J* dims=xs;
    J rnk=dims[0];dims+=1;
    pj(rnk);pf(" ");
    J t=1,d;
    DO(i,rnk,{d=dims[i];t*=d;pj(d);if (i!=rnk-1) {pf(",");}}) nl;
    F* e=xs+(rnk+1)*8;
    PA("%f",t,e);
}

void pai(U xs) {
    J* dims=xs;
    J rnk=dims[0];dims+=1;
    pj(rnk);pf(" ");
    J t=1,d;
    DO(i,rnk,{d=dims[i];t*=d;pj(d);if (i!=rnk-1) {pf(",");}}) nl;
    J* e=xs+(rnk+1)*8;
    PA("%lld",t,e);
}

void pab(U xs) {
    J* dims=xs;
    J rnk=dims[0];dims+=1;
    pj(rnk);pf(" ");
    J t=1,d;
    DO(i,rnk,{d=dims[i];t*=d;pj(d);if(i!=rnk-1){pf(",");}}) nl;
    B* e=xs+(rnk+1)*8;
    PA("%d",t,e);
}
