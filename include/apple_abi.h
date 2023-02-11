#include <stdint.h>
#include <stdbool.h>

#define R return

typedef double F;typedef int64_t I; typedef void* U; typedef bool B;

#define DO(i,n,a) {I i;for(i=0;i<n;i++){a;}}

typedef struct Af { I rnk; I* dim; F* xs; } Af;
typedef struct Ai { I rnk; I* dim; I* xs; } Ai;
