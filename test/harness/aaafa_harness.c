#include <stdio.h>
#include <stdlib.h>

#include"../../include/apple_abi.h"
#include"../../include/apple_p.h"

extern U aaafa(U,U,U,F);

int main(int argc, char *argv[]) {
    F wh[] = {0.51426693,0.56885825,0.48725347,0.15041493};
    F wo[] = {0.14801747,0.37182892};
    F bh[] = {0.79726405,0.67601843};
    F bo = 0.57823076;
    I dwh[] = {2,2};
    I dwo[] = {2};I dbh[] = {2};
    Af a = {2,dwh,wh};
    Af b = {1,dwo,wo};
    Af c = {1,dbh,bh};
    U x = poke_af(a);U y = poke_af(b);U z = poke_af(c);
    paf(aaafa(x,y,z,bo));
    free(x);free(y);free(z);
}
