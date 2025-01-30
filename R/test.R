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

ruffini<-jit("λp.λa. {:((λs.λc. (a*s+c)) Λₒ 0 (p::𝟙𝟘))")
stopifnot(all(run(ruffini,as.integer(c(1,2,1)),-1)==as.integer(c(1,1,0))))

base<-jit("λa.λn. {log ← (%)⑂_.; N ⟜ ⌊((log⑂ℝ) a n)+1; ~(ug. (λs. (s/.n, s|n)) a N)}")
stopifnot(all(run(base,15,4)==as.integer(c(3,3))))

last7<-jit("λas.}. ([x]\\`7 (as::Vec n int))")
stopifnot(all(run(last7,seq(0,9))==seq(3,9)))

any1<-jit("(λa. (λbs. (∨)/ₒ #f bs)`{1∘[2]} (a::M bool))")
stopifnot(all(run(any1,matrix(c(FALSE,FALSE,FALSE,TRUE),2))==c(FALSE,TRUE)))

any<-jit("λbs. (∨)/ₒ #f bs :: bool")
stopifnot(run(any,c(FALSE,FALSE,FALSE,TRUE)))

stopifnot(all(const("(even.'0..2)〃2")==matrix(c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE),2,byrow=TRUE)))

isbn<-jit('xs ↦ (xs⋅(}:(𝔸13⊙7)))|10=0')
stopifnot(run(isbn,as.integer(c(9,7,8,0,5,9,6,5,2,8,1,2,6))));stopifnot(!run(isbn,as.integer(c(9,7,8,1,7,8,8,3,9,9,0,8,3))))
rm(isbn)
gc()

# https://code.jsoftware.com/wiki/Essays/Inverse_Permutation#Boolean_Matrices
bmat<-jit("[x (=)⊗ xᶥ]");pv<-jit("(([x]@.)')")
stopifnot(all(run(pv,run(bmat,as.integer(c(1,0,2))))==as.integer(c(1,0,2))))

prime_mask<-jit("λN. (λn.¬((∨)/ₒ #f ([n|x=0]'2..(⌊(√(ℝn))))))'2..N")
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
m6<-jit("[(x::(Arr (64×64) 𝞈))%.(y::Arr (64×64) 𝞈)]")
stopifnot(all(B%*%C==run(m6,B,C)))

mT6<-jit("[(x::(Arr (64×64) 𝞈))%.|:(y::Arr (64×64) 𝞈)]")
(B%*%t(C))[,55]
run(mT6,B,C)[,55]

covar<-lafile("../math/stats/covar.🍏")
X <- matrix(rnorm(60),15,4);XT<-t(X)
cov(X);run(covar,XT)

hms<-jit("λs. (->1)'({: ((λqr.λb. (qr->2|b, qr->2/.b)) Λₒ (0,s) ⟨24,60,60⟩))")
stopifnot(all(run(hms,86399)==as.integer(c(23,59,59))))
