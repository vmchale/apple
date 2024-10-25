library(zoo)
library(microbenchmark)
a<-seq(0,999,1.0)
source("./apple.R")

sliding_mean<-jit("([((+)/x)%ℝ(:x)]⑄7)")
microbenchmark(rollmean(a,7))
microbenchmark(rollapply(a,7,(mean)))
microbenchmark(run(sliding_mean,a))

x<-runif(10000,0,1);y<-runif(10000,0,1)
microbenchmark(x%*%y)
microbenchmark(sum(x*y))

dp<-jit("[(+)/ ((*)`(x::Vec n float) y)]")
microbenchmark(run(dp,x,y))

A<-matrix(runif(1024,0,1),32);x<-runif(32,0,1)
vmul<-jit("[x::M float%:y]")
microbenchmark(A%*%x)
microbenchmark(run(vmul,A,x))

B<-matrix(runif(4096,0,1),64);C<-matrix(runif(4096,0,1),64)
m6<-jit("[(x::(Arr (64×64) float))%.(y::Arr (64×64) float)]")
microbenchmark(B%*%C)
microbenchmark(run(m6,B,C))

mT6<-jit("[(x::(Arr (64×64) float))%.|:(y::Arr (64×64) float)]")
microbenchmark(B%*%t(C))
microbenchmark(run(mT6,B,C))
