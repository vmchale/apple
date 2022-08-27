#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../../c/apple.c"

extern F aaf(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {1,2,3};
    F ys[] = {2,4,6};
    I d[] = {3};
    Af a = {1,d,xs};
    Af b = {1,d,ys};
    U x = poke_af(a);
    U y = poke_af(b);
    printf("%f\n", aaf(x, y));
    free(x);free(y);
}
