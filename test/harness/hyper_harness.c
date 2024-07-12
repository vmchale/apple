#include<stdio.h>
#include<hyper.h>

int main(int argc, char *argv[]) {
    F xs[] = {1};
    F ys[] = {1.5};
    J d[] = {1};
    Af a = {1,d,xs};
    Af b = {1,d,ys};
    printf("%f", hyper_wrapper(a, b, 1));
}
