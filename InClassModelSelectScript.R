x <- matrix(rnorm(80),ncol=8)
colnames(x) <- paste("X",1:8,sep="")
y <- rnorm(10)

g <- 3
parallel <- TRUE
library(multicore)
library(doMC)
library(foreach)
registerDoMC(cores=10)

allNothing <- c("X1","X2")
eitherOr <- c("X3","X4")
always <- "X5"

###########################################

modelSelect(varNames=colnames(x),
                                       always=always, 
                                       allNothing=allNothing, 
                                       eitherOr=eitherOr,
                                       parallel=parallel)
                                       
###########################################

alwaysCondition <- TRUE

allNothingCondition <- c(TRUE, FALSE)

otherrestrictedsList

restrictedsList

head(restrictedsModels)

eitherOrTestResults

head(restrictedsMatrix)

unrestrictedsList

head(unrestrictedsMatrix)

head(modelMatrix)

###########################################

modelSelect(varNames=colnames(x),
                                       always=always, 
                                       allNothing=allNothing, 
                                       eitherOr=eitherOr,
                                       parallel=parallel)