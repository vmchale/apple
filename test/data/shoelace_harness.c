#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef double F;typedef int64_t I; typedef void* U;

#define V(n,xs,p) {p=malloc(16+8*n);I* i_p=p;*i_p=1;i_p[1]=n;memcpy(p+16,xs,8*n);}

extern F shoelace(U, U);

int main(int argc, char *argv[]) {
    F xs[] = {0,4,4};
    F ys[] = {0,0,3};
    U x; V(3,xs,x);
    U y; V(3,ys,y);
    printf("%f\n", shoelace(x,y));
    free(x);free(y);
}
