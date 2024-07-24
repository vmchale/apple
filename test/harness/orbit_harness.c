#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_p.h"

extern U orbit(F,F);

int main(int argc, char *argv[]) {
    F a=2.576; F b=4.628;
    paf(orbit(a,b));
}
