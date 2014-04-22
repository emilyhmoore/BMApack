x <- matrix(rnorm(220), ncol=11)
colnames(x) <- paste("X", 1:11,sep="")
y <- rnorm(20)
allNothing <- list(c("X1","X2"))
eitherOr <- list(c("X3", "X4"))
always <- "X5"
conditionals <- list(c("X6", "X7"))
conditionedOnTheseVariables <- list(c("X8","X9"))
parallel <- FALSE
g <- 3

BMAObject <- fitBMA(x, y, g, parallel, allNothing, eitherOr, always,conditionals,conditionedOnTheseVariables)

BMAObject

summary(BMAObject)

plot(BMAObject)







