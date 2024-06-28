#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_p.h"

extern U aaa(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {1,0,0};
    F ys[] = {2,3,4,5,6,9};
    J dx[] = {3};
    J dy[] = {3,2};
    Af a = {1,dx,xs};
    Af b = {2,dy,ys};
    U x = poke_af(a);
    U y = poke_af(b);
    paf(aaa(x,y));
    free(x);free(y);
}
