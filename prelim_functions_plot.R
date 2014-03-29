data<-matrix(rnorm(1000), ncol=10)
colnames(data)<-c(paste("x", 1:10, sep=""))
datay<-5*data[,2]+3*data[,3]+rnorm(100)
BMAobject<-(fitBMA(x=data, y=datay, g=3, parallel=FALSE)) 


BMAtheses <- BMAobject@theses
BMAtheses[is.na(BMAtheses)] <- 0
BMAtheses


BMAthecoefs <- BMAobject@thecoefs
BMAthecoefs[is.na(BMAthecoefs)] <- 0
BMAthecoefs

ceofslist.fun <- function(i){
  coefslist <- list(NULL)
  coefslist <- list(matrix(BMAthecoefs[,i]))
  return(coefslist)
}
coefslist <- aaply(1:ncol(BMAthecoefs), .margins=1, .fun=ceofslist.fun)

seslist.fun <- function(i){
  seslist <- list(NULL)
  seslist <- list(matrix(BMAtheses[,i]))
  return(seslist)
}
seslist <- aaply(1:ncol(BMAtheses), .margins=1, .fun=seslist.fun)

cbind.fun <- function(i){
  cbindmat <- list(NULL)
  cbindmat <- cbind(coefslist[[i]], seslist[[i]], modelodds)
  return(cbindmat)
}

totalmat <- llply(1:length(coefslist), .fun=cbind.fun)

zeroelim.fun <- function(i){
  nonzeromat <- list(NULL)
  nonzeromat <- totalmat[[i]][rowSums(totalmat[[i]]==0)==0,]
  return(nonzeromat)
}

nonzeromat <- llply (1:length(totalmat), .fun=zeroelim.fun)

#####################
nonzeromat1 <- nonzeromat[[1]]
nonzeromat1

dnorm.fun <- function(i){
  dnormmat <- matrix()
  dnormmat <- dnorm(seq(min(nonzeromat1[,1])-4*max(nonzeromat1[,2]), 
                       
                 max(nonzeromat1[,1])+4*max(nonzeromat1[,2]), length=100),  
                 
                 mean=nonzeromat1[i,1], sd=nonzeromat1[i,2])
  return(dnormmat)
}

dnormmat <- t(aaply(1:nrow(nonzeromat1), .fun=dnorm.fun, .margins=1))

dnormmat <- dnormmat %*% nonzeromat1[,3]

plot(seq(min(nonzeromat1[,1])-4*max(nonzeromat1[,2]), 
         
         max(nonzeromat1[,1])+4*max(nonzeromat1[,2]), length=100), dnormmat, xlab="", ylab="", type="l")


#need to figure out how to apply this over entire list

############################
