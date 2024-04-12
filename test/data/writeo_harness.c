#include<stdio.h>
#include"shoelace.h"

int main(int argc, char *argv[]) {
    F xs[] = {0,4,4};
    F ys[] = {0,0,3};
    I d[] = {3};
    Af a = {1,d,xs};
    Af b = {1,d,ys};
    printf("%f\n", shoelace_wrapper(a,b));
}