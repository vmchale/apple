source("./apple.R")
library(readr)

ncdf_str<-read_file("../math/ncdf.🍎")
ncdf<-jit(ncdf_str)
run(ncdf,3);pnorm(3)

chisqcdf_str<-read_file("../math/chisqcdf.🍎")
chisqcdf<-jit(chisqcdf_str)
run(chisqcdf,2,2);pchisq(2,2)

tcdf_str<-read_file("../math/tcdf.🍎")
tcdf<-jit(tcdf_str)
run(tcdf,2,12);pt(2,12)
