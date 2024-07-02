#include <stdio.h>
#include<string.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

extern J ai(U);

int main(int argc, char *argv[]) {
    F xs[] = {1,2,3,4,5,6,7,8,9,10};
    U x; V(6,xs,x);
    printf("%lld\n", ai(x));
    free(x);
}
