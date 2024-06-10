#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

extern U aa(U);

void paf(U xs) {
    I* dims=xs;
    I rnk=dims[0];
    printf("%lld;",rnk);
    I t=1;
    I d;
    for(I i=1;i<=rnk;i++) {
        d=dims[i];
        t*=d;
        printf("%lld",d); if (i!=rnk) {printf(",");}
    }
    printf("\n");
    F* e=xs+d+1;
    DO(i,t,{
        printf("%f",e[i]); if (i!=t-1) {printf(", ");}
    })
    printf("\n");
}

int main(int argc, char *argv[]) {
    F xs[] = {0,1,1,0};
    I dx[] = {2,2};
    Af a = {2,dx,xs};
    U x = poke_af(a);
    paf(aa(x));
    free(x);
}
