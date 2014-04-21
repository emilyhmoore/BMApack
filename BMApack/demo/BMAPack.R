x <- matrix(rnorm(600),ncol=20)
colnames(x) <- paste("X",1:20,sep="")
y <- rnorm(30)
allNothing <- list(c("X1","X2"),c("X3","X4"))
eitherOr <- list(c("X5","X6"),c("X7","X8"))
always <- c("X9","X10")
conditionals <- list(c("X11","X12"),c("X13","X14"))
conditionedOnTheseVariables <- list(c("X15","X16"),c("X17","X18"))

g <- 3
parallel <- TRUE
library(multicore)
library(doMC)
library(foreach)
registerDoMC(cores=2)

BMAObject <- fitBMA(x, y, g, parallel, allNothing, eitherOr, always,conditionals,conditionedOnTheseVariables)

BMAObject

plot(BMAObject)

