#include <stdio.h>
#include <stdlib.h>

extern double ff(double);

#define PI 3.14159265358979

int main(int argc, char *argv[]) {
    printf("%f\n", ff(3*PI/2));
}
