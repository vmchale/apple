#include <stdio.h>
#include <stdlib.h>

extern double fff(double, double);

int main(int argc, char *argv[]) {
    printf("%f\n", fff(1.0,12.0));
}
