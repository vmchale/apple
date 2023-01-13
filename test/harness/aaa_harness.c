#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../../c/apple.c"

extern U aaa(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {0,4,4};
    F ys[] = {0,0,3};
    I d[] = {3};
    Af a = {1,d,xs};
    Af b = {1,d,ys};
    U x = poke_af(a);
    U y = poke_af(b);
    printf("%p\n", aaa(x, y));
    free(x);free(y);
}
