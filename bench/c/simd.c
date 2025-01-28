#include<stdlib.h>

double amax(double* xs, size_t n){
    double x,y=-100;
    for(int i=0;i<n;i++) {
        x=xs[i];
        y=x>y?x:y;
    }
    return y;
}

double asum(double* xs, size_t n){
    double y=0;
    for(int i=0;i<n;i++) {
        y+=xs[i];
    }
    return y;
}

double* ainv(double* xs, size_t n) {
    double* ys=malloc(sizeof(double)*n);
    for(int i=0;i<n;i++) {
        ys[i]=1/(1+xs[i]);
    }
    return ys;
}
