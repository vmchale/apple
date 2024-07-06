#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_p.h"

extern U conv(U);

int main(int argc, char *argv[]) {
    F xs[] = {9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9};
    J dx[] = {4,4};
    Af a = {2,dx,xs};
    U x = poke_af(a);
    paf(conv(x));
    free(x);
}
