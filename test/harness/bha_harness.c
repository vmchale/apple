#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_p.h"

extern U aaa(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {0.79726405,0.67601843};
    F ys[] = {-0.00461325,-0.0120947,0.00140493,0.00440132,0.00133441,0.00348059,-0.00259046,-0.00849969};
    J dx[] = {2};
    J dy[] = {4,2};
    Af a = {1,dx,xs};
    Af b = {2,dy,ys};
    U x = poke_af(a);
    U y = poke_af(b);
    paf(aaa(x,y));
    free(x);free(y);
}
