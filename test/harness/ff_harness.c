#include <stdio.h>
#include <stdlib.h>

extern double ff(double);

int main(int argc, char *argv[]) {
    printf("%f\n", ff(1.0));
}
