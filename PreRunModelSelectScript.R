fitBMA(x,y,g,parallel,allNothing,eitherOr,always)

modelSelect(varNames,parallel,allNothing,eitherOr,always)

## 6570x22 Dataset
load("/Users/jaeheejung/Desktop/BMApack/BMApack/data/OOS.rda")

y <- OOS[,1]
x <- OOS[,-1]

g <- 3
parallel <- TRUE
library(multicore)
library(doMC)
library(foreach)
registerDoMC(cores=20)

allNothing <- list(c("eqada2","new.inter","pvr2base"),c("folded1","new.inter2"))
eitherOr <- list(c("chql","frrun","new.sen"),c("inparty","incr2"))
always <- c("popr2","midr2","year") 

##########################################

varNames <- colnames(x)


restricteds <- c(unlist(allNothing),always,unlist(eitherOr))


restrictedsIndex <- which(varNames%in%restricteds)


unrestricteds <- varNames[-restrictedsIndex]


alwaysCondition <- TRUE


allNothingCondition<-          replicate(length(allNothing),list(c(TRUE, FALSE)))


restrictedsList<-c(alwaysCondition=alwaysCondition,                allNothingCondition=allNothingCondition)


allNothingConditionNames <- names(restrictedsList)[-1]


otherrestricteds <- restricteds[-c(which(restricteds%in%always),                                         which(restricteds%in%unlist(allNothing)))]
 
 
otherrestrictedsList<-list()

if(length(otherrestricteds)!=0){
		          
otherrestrictedsList <- llply(1:length(otherrestricteds),
                                          function(i){
                                          	otherrestrictedsList[[i]]<-c(TRUE, FALSE)
                                          	},
  
  .parallel=parallel)
           }
     
        
names(otherrestrictedsList)<-otherrestricteds


restrictedsList<-c(restrictedsList, otherrestrictedsList)


restrictedsModels <- expand.grid(restrictedsList)


restrictedsMatrix <-matrix(rep(FALSE),ncol=length(c(unlist(allNothing), always)), nrow=nrow(restrictedsModels))
		      
		      
colnames(restrictedsMatrix)<-c(unlist(allNothing), always)
		    
		    
restrictedsMatrix[,always]<-restrictedsModels[,"alwaysCondition"]


for(i in 1:length(allNothing)){
		restrictedsMatrix[,allNothing[[i]]]<-restrictedsModels[,allNothingConditionNames[i]]
		
		}

                    
restrictedsMatrix<-cbind(restrictedsMatrix,restrictedsModels[,otherrestricteds])
        

eitherOrTest <- function(x){length(which(x==TRUE))==1 | any(as.logical(x))==FALSE}
             
              
eitherOrTestResults <- NULL
for(i in 1:length(eitherOr)){
              	eitherOrStripIndex <- unlist(alply(restrictedsMatrix[eitherOr[[i]]], 1, eitherOrTest))
              	eitherOrTestResults <- c(eitherOrTestResults,eitherOrStripIndex)
              	}              


eitherOrTestResults <- matrix(eitherOrTestResults,ncol=length(eitherOr),byrow=FALSE)


eitherOrTestResultsCombined <- aaply(eitherOrTestResults,1,
              function(x){
              	ifelse(all(x),TRUE,FALSE)
              }) 
              
                           
restrictedsMatrix <- restrictedsMatrix[eitherOrTestResultsCombined,]

           
bindTogether <- function(i){cbind(unrestrictedsMatrix[unrestrictedsMatrix$temp==i,], restrictedsMatrix[i,])}

unrestrictedsList<-list()

if(length(unrestricteds)!=0){
              
         length(unrestrictedsList) <- length(unrestricteds)
         
  		 unrestrictedsList <- llply(1:length(unrestricteds), 
  		                                    function(i){unrestrictedsList[[i]]<-c(TRUE, FALSE)},
  		              
  		 .parallel=parallel)
  		                                    
  		 unrestrictedsList <- c(unrestrictedsList, list(temp=1:nrow(restrictedsMatrix)))
  		 
         names(unrestrictedsList) <- c(unrestricteds, "temp")
                
         unrestrictedsMatrix <- expand.grid(unrestrictedsList)
                
         modelMatrix <- do.call("rbind",llply(1:nrow(restrictedsMatrix), bindTogether))
              
         modelMatrix$temp<-NULL
              
         modelMatrix <- modelMatrix[colnames(x)]
              
  		        }else{
  		 
  		 modelMatrix <- restrictedsMatrix
  		        	
  		 modelMatrix <- modelMatrix[colnames(x)]
  		        
  		        }



