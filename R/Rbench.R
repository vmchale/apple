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

dp<-jit("[(+)/ ((*)`(x::Vec n float) y)]")
microbenchmark(run(dp,x,y))
