#include <stdio.h>
#include<string.h>
#include <stdlib.h>

#include"../../include/apple_p.h"

extern U a(U);

int main(int argc, char *argv[]) {
    F xs[] = {1,3,2,5};
    V(4,xs,x);
    paf(a(x));
    free(x);
}
