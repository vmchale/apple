source("./apple.R")
library(readr)

set.seed(17)

lafile<-function(f){str<-read_file(f);jit(str)}

ncdf<-lafile("../math/ncdf.🍎")
run(ncdf,3);pnorm(3)

chisqcdf<-lafile("../math/chisqcdf.🍎")
run(chisqcdf,2,2);pchisq(2,2)

tcdf<-lafile("../math/tcdf.🍎")
run(tcdf,2,12);pt(2,12)

sliding_mean<-jit("([(+)/x%ℝ(:x)]\\`7)")
stopifnot(all(run(sliding_mean,seq(0,10,1.0))==c(3,4,5,6,7)))

cat<-jit("[x++(y::Vec n int)]")
stopifnot(all(run(cat,as.integer(c(1,1)),as.integer(c(0,2,3)))==c(1,1,0,2,3)))

any1<-jit("(λa. (λbs. (∨)/ₒ #f bs)`{1∘[2]} (a::M bool))")
stopifnot(all(run(any1,matrix(c(FALSE,FALSE,FALSE,TRUE),2))==c(FALSE,TRUE)))

any<-jit("λbs. (∨)/ₒ #f bs :: bool")
stopifnot(run(any,c(FALSE,FALSE,FALSE,TRUE)))

stopifnot(all(const("(even.'irange 0 2 1)〃2")==matrix(c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE),2,byrow=TRUE)))

isbn<-jit('λxs. ((+)/ (*)`xs (}:(𝔸13⊙7)))|10=0')
stopifnot(run(isbn,as.integer(c(9,7,8,0,5,9,6,5,2,8,1,2,6))));stopifnot(!run(isbn,as.integer(c(9,7,8,1,7,8,8,3,9,9,0,8,3))))
rm(isbn)
gc()

prime_mask<-jit("λN. (λn.¬((∨)/ₒ #f ([n|x=0]'⍳ 2 (⌊(√(ℝn))) 1)))'irange 2 N 1")
stopifnot(all(run(prime_mask,9)==c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE)))

fibs<-jit("λN. [x˙0˙1]'{A⟜⟨⟨1,1⟩,⟨1,0::int⟩⟩; gen. A (A%.) N}")
stopifnot(all(run(fibs,6)==c(1,1,2,3,5,8)))

A<-matrix(runif(32,0,1),4);x<-runif(8,0,1)
mul<-jit("[x::M float%:y]")
run(mul,A,x)
(A%*%x)[,1]

x<-runif(128,0,1);y<-runif(128,0,1)
dp<-jit("[(+)/(*)`(x::Vec n float) y]")
(x%*%y)[,1]
run(dp,x,y)
# LOL stopifnot((x%*%y)[,1]==sum(x*y))

B<-matrix(runif(4096,0,1),64);C<-matrix(runif(4096,0,1),64)
m6<-jit("[(x::(Arr (64×64) float))%.(y::Arr (64×64) float)]")
stopifnot(all(B%*%C==run(m6,B,C)))

mT6<-jit("[(x::(Arr (64×64) float))%.|:(y::Arr (64×64) float)]")
(B%*%t(C))[,55]
run(mT6,B,C)[,55]

covar<-lafile("../math/stats/covar.🍏")
X <- matrix(rnorm(60),15,4);XT<-t(X)
cov(X);run(covar,XT)
