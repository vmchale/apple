#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

extern F af(U);

int main(int argc, char *argv[]) {
    F xs[] = {1,2,3};
    U x; V(3,xs,x);
    printf("%f\n", af(x));
    free(x);
}
