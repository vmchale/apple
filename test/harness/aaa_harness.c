#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

extern U aaa(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {0,1,1,1};
    F ys[] = {1,1};
    I dx[] = {2,2};
    I dy[] = {2};
    Af a = {2,dx,xs};
    Af b = {1,dy,ys};
    U x = poke_af(a);
    U y = poke_af(b);
    printf("%p\n", aaa(x, y));
    free(x);free(y);
}
