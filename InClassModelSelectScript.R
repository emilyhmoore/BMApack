load("/Users/jaeheejung/Desktop/Spring 2014/Applied Statistical Programming/BMApack/BMApack/data/ABDagg.rda")

x <- ABDagg[]
y <- ABDagg[]

g <- 3
parallel <- TRUE
library(multicore)
library(doMC)
library(foreach)
registerDoMC(cores=10)

allNothing <- c()
eitherOr <- c()
always <- 

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