#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_p.h"

extern U aafa(U,U,F);

int main(int argc, char *argv[]) {
    F ho[] = {0.68938893, 0.66284947, 0.78321778, 0.69560025, 0.78776923, 0.77641173, 0.8580009, 0.80143567};
    F wo[] = {0.14801747,0.37182892};
    F bo = 0.57823076;
    I dho[] = {4,2};
    I dwo[] = {2};
    Af a = {2,dho,ho};
    Af b = {1,dwo,wo};
    U x = poke_af(a);U y = poke_af(b);
    paf(aafa(x,y,bo));
    free(x);free(y);
}
