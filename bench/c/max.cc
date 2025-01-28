#include<algorithm>

double amax(double*,size_t) asm ("_maxf");
double amax(double* xs, size_t n) {
    double x,y=xs[0];
    for(int i=1;i<n;i++) {
        y=std::fmax(y,xs[i]);
    }
    return y;
}
