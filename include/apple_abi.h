#include <stdint.h>

#define R return
#define I int64_t
#define F double
#define U void*
#define B bool

#define DO(i,n,a) {I i;for(i=0;i<n;i++){a;}}

typedef struct Af { I rnk; I* dim; F* xs; } Af;
typedef struct Ai { I rnk; I* dim; I* xs; } Ai;
