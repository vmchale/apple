library(microbenchmark)
source("./apple.R")

X<-runif(2000,0,1);Y<-runif(2000,0,1)
cosim<-jit("λa.λb. a⋅b%(√((+)/((^2)'a))*√((+)/((^2)'b)))")
microbenchmark(run(cosim,X,Y))

library(lsa)
microbenchmark(cosine(X,Y))

R_cosim<-function(X,Y){sum(X*Y)/(sqrt(sum(X^2))*sqrt(sum(Y^2)))}
microbenchmark(R_cosim(X,Y))
