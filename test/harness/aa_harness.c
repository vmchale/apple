#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"
#include"../../include/apple_p.h"

extern U aa(U);

int main(int argc, char *argv[]) {
    F xs[] = {0,1,1,0};
    I dx[] = {2,2};
    Af a = {2,dx,xs};
    U x = poke_af(a);
    paf(aa(x));
    free(x);
}