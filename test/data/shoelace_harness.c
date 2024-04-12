#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef double F;typedef int64_t I; typedef void* U;

extern F shoelace(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {0,4,4};
    F ys[] = {0,0,3};
    U x=malloc(40);U y=malloc(40);
    I* i_x=x;I* i_y=y;
    *i_x=1;*i_y=1;
    i_x[1]=3;i_y[1]=3;
    memcpy(x+16,xs,24);
    memcpy(y+16,ys,24);
    printf("%f\n", shoelace(x,y));
    free(x);free(y);
}
