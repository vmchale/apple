#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../../c/apple.c"

extern F af(U);

int main(int argc, char *argv[]) {
    F xs[] = {1,2,3};
    I d[] = {3};
    Af a = {1, d, xs};
    U x = poke_af(a);
    printf("%f\n", af(x));
    free(x);
}
