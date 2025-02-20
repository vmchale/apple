#include <stdio.h>
#include<string.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

extern B ab(U);

int main(int argc, char *argv[]) {
    B xs[] = {false,false,true,false,true,false};
    V(6,xs,x);
    printf("%d\n", ab(x));
    free(x);
}
