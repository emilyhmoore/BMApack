library(devtools)
library(roxygen2)
find_rtools() ##For using devtools with windows. 
setwd("~/GitHub/BMAPack")
create(path="./BMApack", check=FALSE)

current.code <- as.package("BMAPack")
load_all(current.code)
document(current.code)


 x1<-rnorm(500)
 x2<-rnorm(500,3,15)
 dep<-(x1+2*x2)+rnorm(500,4,100)
 covars<-cbind(x1,x2) 
jj <- fitBMA(x=covars, y=dep, parallel=FALSE)
str(jj)

install(pkg=current.code, local=TRUE)

check(current.code)

##Example data

help(fitBMA)
help.search('summary,bma-method')
example(fitBMA)
example('summary,bma-method')
example('plot,bma-method') ##may take a sec


