library(microbenchmark)
source("./apple.R")

x<-runif(10000,0,1);y<-runif(10000,0,1)
microbenchmark(x%*%y)
microbenchmark(sum(x*y))

dp<-jit("[(+)/(*)`(x::𝟙𝞈) y]")
microbenchmark(run(dp,x,y))

A<-matrix(runif(1024,0,1),32);x<-runif(32,0,1)
vmul<-jit("[x::M𝞈%:y]")
microbenchmark(A%*%x)
microbenchmark(run(vmul,A,x))

msz <- function(M,N,K){
    print(c(M,N,K))
    A<-matrix(runif(M*N,0,1),M);B<-matrix(runif(N*K,0,1),K)
    m<-jit("[x%.(y::M 𝞈)]")
    print(microbenchmark(run(m,A,B)));print(microbenchmark(A%*%B))
}

library(glue)
mT <- function(M,N,K){
    print(c(M,N,K))
    A<-matrix(runif(M*N,0,1),M);B<-matrix(runif(K*N,0,1),N)
    mT<-jit(glue::glue("[(x::(Arr ({M}×{N}) 𝞈))%.|:(y::Arr ({K}×{N}) 𝞈)]"))
    print(microbenchmark(run(mT,A,B)));print(microbenchmark(A%*%t(B)))
}
mT(64,64,64);msz(64,64,64);mT(512,512,512);mT(1024,1024,1024)
