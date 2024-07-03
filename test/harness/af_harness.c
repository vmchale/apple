#include <stdio.h>
#include<string.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

extern F af(U);

int main(int argc, char *argv[]) {
    F xs[] = {4,8,8,8,8,8};
    U x; V(6,xs,x);
    printf("%f\n", af(x));
    free(x);
}
