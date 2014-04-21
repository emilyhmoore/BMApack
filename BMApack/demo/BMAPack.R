x <- matrix(rnorm(80),ncol=8)
colnames(x) <- paste("X",1:8,sep="")
y <- rnorm(10)
allNothing <- c("X1","X2")
eitherOr <- c("X5","X6")
always <- c("X3","X4")
fitBMA(x=x, y=y, allNothing=allNothing, eitherOr=eitherOr, always=always)

plot(fitBMA(x=x, y=y, allNothing=allNothing, eitherOr=eitherOr, always=always))

