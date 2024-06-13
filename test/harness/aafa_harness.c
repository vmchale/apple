#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"
#include"../../include/apple_p.h"

extern U aafa(U,U,F);

int main(int argc, char *argv[]) {
    F wh[] = {0.51426693,0.56885825,0.48725347,0.15041493};
    F wo[] = {0.14801747,0.37182892};
    F bo = 0.57823076;
    I dwh[] = {2,2};
    I dwo[] = {2};I dbh[] = {2};
    Af a = {2,dwh,wh};
    Af b = {1,dwo,wo};
    U x = poke_af(a);U y = poke_af(b);
    paf(aafa(x,y,z,bo));
    free(x);free(y);
}
