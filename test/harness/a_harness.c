#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"

extern U a();

int main(int argc, char *argv[]) {
    printf("%p\n", a());
}
