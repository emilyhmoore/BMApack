library(devtools)
library(roxygen2)
find_rtools() ##For using devtools with windows. 
setwd("~/GitHub/BMAPack")
create(path="./BMApack", check=FALSE)

current.code <- as.package("BMAPack")
load_all(current.code)
document(current.code)

g <- 3
parallel <- TRUE
library(multicore)
library(doMC)
library(foreach)
registerDoMC(cores=2)

x <- matrix(rnorm(220), ncol=11)
colnames(x) <- paste("X", 1:11,sep="")
y <- rnorm(20)
allNothing <- list(c("X1","X2"))
eitherOr <- list(c("X3", "X4"))
always <- "X5"
conditionals <- list(c("X6", "X7"))
conditionedOnTheseVariables <- list(c("X8","X9"))

bmaObject <- fitBMA(x,y,g,parallel,allNothing,eitherOr,always,conditionals,conditionedOnTheseVariables)
summary(bmaObject)
plot(bmaObject)

install(pkg=current.code, local=TRUE)

check(current.code)

##Example data

help(fitBMA)
help.search('summary,bma-method')
example(fitBMA)
example('summary,bma-method')
example('plot,bma-method') ##may take a sec


