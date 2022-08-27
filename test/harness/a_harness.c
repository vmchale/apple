#include <stdio.h>
#include <stdlib.h>

extern void* a(void);

int main(int argc, char *argv[]) {
    printf("%p\n", a());
}
