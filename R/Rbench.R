library(zoo)
library(microbenchmark)
a<-seq(0,999,1.0)
source("./apple.R")

sliding_mean<-jit("([((+)/x)%ℝ(:x)]\\`7)")
microbenchmark(rollmean(a,7))
microbenchmark(run(sliding_mean,a))
