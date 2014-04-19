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


varNames <- colnames(x)


restricteds<-c(allNothing,always,eitherOr)


restrictedsIndex <- which(varNames%in%restricteds)


unrestricteds <- varNames[-restrictedsIndex]


alwaysCondition <- TRUE


allNothingCondition<-c(TRUE, FALSE)


restrictedsList<-list(alwaysCondition=alwaysCondition,allNothingCondition=allNothingCondition)


otherrestricteds<-restricteds[-c(which(restricteds%in%always),                                     which(restricteds%in%allNothing))]
 
 
otherrestrictedsList<-list()

if(length(otherrestricteds)!=0){
	       
otherrestrictedsList<-llply(1:length(otherrestricteds),

function(i){
	otherrestrictedsList[[i]]<-c(TRUE, FALSE)
	},
      .parallel=parallel)
                                          
        }
     
        
names(otherrestrictedsList)<-otherrestricteds


restrictedsList<-c(restrictedsList, otherrestrictedsList)


restrictedsModels <- expand.grid(restrictedsList)


restrictedsMatrix <-matrix(rep(FALSE),ncol=length(c(allNothing, always)),nrow=nrow(restrictedsModels))
		      
		      
colnames(restrictedsMatrix) <- c(allNothing, always)
		    
		    
restrictedsMatrix[,always]<-restrictedsModels[,"alwaysCondition"]


restrictedsMatrix[,allNothing]<-restrictedsModels[,"allNothingCondition"]
          
          
restrictedsMatrix<-cbind(restrictedsMatrix,restrictedsModels[,otherrestricteds])
        

eitherOrTest<-function(x){length(which(x==TRUE))==1 | any(as.logical(x))==FALSE}
             
              
eitherOrTestResults<-unlist(alply(restrictedsMatrix[eitherOr], 1, eitherOrTest))
              
              
restrictedsMatrix<-restrictedsMatrix[eitherOrTestResults,]

              
unrestrictedsList<-list()
  		    
if(length(unrestricteds)!=0){
	length(unrestrictedsList) <- length(unrestricteds)
    
    unrestrictedsList<-llply(1:length(unrestricteds), 
  	
  	function(i){unrestrictedsList[[i]] <- c(TRUE, FALSE)},.parallel=parallel)
    
    unrestrictedsList <- c(unrestrictedsList, list(temp=1:nrow(restrictedsMatrix)))
    
    names(unrestrictedsList) <- c(unrestricteds, "temp")
  		        
  		        }
  

unrestrictedsMatrix <- expand.grid(unrestrictedsList)
         
         
bindTogether <- function(i){cbind(unrestrictedsMatrix[unrestrictedsMatrix$temp==i,], restrictedsMatrix[i,])}
          

modelMatrix <- do.call("rbind",llply(1:nrow(restrictedsMatrix), bindTogether))
          
          
modelMatrix$temp <- NULL
          
         
modelMatrix <- modelMatrix[colnames(x)]
