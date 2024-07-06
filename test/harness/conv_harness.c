#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_p.h"

extern U conv(U);

int main(int argc, char *argv[]) {
    F xs[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24};
    J dx[] = {5,5};
    Af a = {2,dx,xs};
    U x = poke_af(a);
    paf(conv(x));
    free(x);
}
