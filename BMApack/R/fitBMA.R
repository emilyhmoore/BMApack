#' fitBMA Function
#'
#' Runs regression on all possible combinations of covariates given the restrictions specified by the user
#'
#' @param x: A numeric matrix of covariates
#' @param y: A numeric vector of the dependent variable
#' @param g: A value for the hyper-prior g
#' @param parallel: runs in parallel if TRUE
#' @param allNothing: a list of the names of covariates that should all be included or none should be included
#' @param eitherOr: a list of the names of covariates among which only one should be included or none should be included
#' @param always: a vector of the names of covariates that should always be included
#' @param conditionals: a list of the names of covariates conditioned on the covariates included in conditionedOnTheseVariables. These may include squared terms or interaction terms.
#' @param conditionedOnTheseVariables: a list of the names of covariates that serve as constitutive terms for covariates included in conditionals.
#'
#' @return An bma class object with the following slots: 
#'  \item{x}{A matrix of covariates}
#'  \item{y}{A vector of the dependent variable} 
#'  \item{coefs}{Coefficients in all models}
#'  \item{standardErrors}{Standard errors of coefficients in all models}
#'  \item{r2s}{R squared values for all models}
#'  \item{postProb}{Posterior probability of each model}
#'  \item{postProbcoefs}{Posterior probability that the coefficient is included}
#'  \item{bfVec}{Bayes factor of each model}
#'  \item{expB}{Expected values of coefficients}
#'  \item{expBcond}{Expected values of coefficients conditional on the variable being included}
#'  \item{largerZero}{Conditional posterior probability that the coefficient is larger than zero}
#'  \item{condSE}{Standard error of coefficients conditional on the variable being included}
#'  \item{coefMatrix}{Matrix of coefficients}
#'  \item{sdMatrix}{Matrix of standard errors}
#' @author Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily Moore
#' @examples
#' 
#' x <- matrix(rnorm(220), ncol=11)
#' colnames(x) <- paste("X", 1:11,sep="")
#' y <- rnorm(20)
#' allNothing <- list(c("X1","X2"))
#' eitherOr <- list(c("X3", "X4"))
#' always <- "X5"
#' conditionals <- list(c("X6", "X7"))
#' conditionedOnTheseVariables <- list(c("X8","X9"))
#' fitBMA(x, y, g=3, parallel=FALSE, allNothing, eitherOr, always,conditionals,conditionedOnTheseVariables)
#' @rdname fitBMA
#' @export

setGeneric(name="fitBMA",
           def=function(x, y, g=3, parallel=TRUE,allNothing=NULL, eitherOr=NULL, always=NULL, 
                        conditionals=NULL, conditionedOnTheseVariables=NULL)
           {standardGeneric("fitBMA")}
           ) 


setMethod(f="fitBMA",
          definition=function(x, y, g=3, 
                              parallel=FALSE,
                              allNothing=NULL, 
                              eitherOr=NULL,
                              always=NULL,
                              conditionals=NULL,
                              conditionedOnTheseVariables=NULL
                              )
        {
            
            ##The restricteds object contains variables that are conditioned.
  		        restricteds<-c(unlist(allNothing),
                             always,
                             unlist(eitherOr), 
                             unlist(conditionals), 
                             unlist(conditionedOnTheseVariables))     
            
            ##This runs to calculate modelMatrix when there are no restrictions.
            if(length(restricteds)==0){
            	
            ##Extract the names of the independent variables, which will be used in later functions.
            varNames <- colnames(x)
            
            ##Make the set of all possible combinations of variables
            ##Returns a list for now.
            varList <- list()  ## an empty list
            for(i in colnames(x)){
              varList <- c(varList,list(i=c(FALSE,TRUE)))
            }
            names(varList) <- colnames(x)

            ##varList is now a list with labels the same as 'x' containing FALSE and TRUE for each variables
            modelMatrix <- as.matrix(expand.grid(varList, KEEP.OUT.ATTRS=FALSE)) ## The complete list of all possible models
            modelMatrix <- modelMatrix[-1, ] ##Remove the null model here

            }else{
                
            ##The modelSelect function returns the correct model configurations.  
            modelSelect<-function(varNames=colnames(x), 
                                    parallel=FALSE,
                                    allNothing, 
                                    eitherOr,
                                    always,
                                    conditionals,
                                    conditionedOnTheseVariables
                                    )
              { 

              
  		        ##restrictedsIndex returns the index of the conditioned variables. 
              ##This is necessary because we want to separate the conditioned ones 
              ##from the unconditioned variables.
  		        restrictedsIndex <- which(varNames%in%restricteds)
  		        unrestricteds <- varNames[-restrictedsIndex]

		          ##The always condition is always included.
		          alwaysCondition <- TRUE
		          
		          ##The allNothing condition is either included or not included.
		          allNothingCondition<-replicate(length(allNothing),list(c(TRUE, FALSE)))
		          
		          ##The restrictedsList is all configurations for the conditioned variables combined.
		          restrictedsList<-c(alwaysCondition=alwaysCondition, 
                                    allNothingCondition=allNothingCondition)
              
              ##Save the list names of the allNothingConditions in order to use them as indices later.
              allNothingConditionNames <- names(restrictedsList)[-1]
          
              ##This is all restricteds that are not always or allNothing types
  		        otherrestricteds<-restricteds[-c(which(restricteds%in%always), 
                                                which(restricteds%in%unlist(allNothing)))]
          
              ##Make a list for the variables that are just going to be true false before they are 
              ##stripped away
              otherrestrictedsList<-list()
               if(length(otherrestricteds)!=0){

		          otherrestrictedsList<-llply(1:length(otherrestricteds),
                                          function(i){otherrestrictedsList[[i]]<-c(TRUE, FALSE)},
                                          .parallel=parallel)
                                          }
                                      
              names(otherrestrictedsList)<-otherrestricteds
          
              restrictedsList<-c(restrictedsList, otherrestrictedsList)

              ##Expand grid on the restricted variables.
              restrictedsModels <- expand.grid(restrictedsList)

			
              restrictedsMatrix <-matrix(rep(FALSE),ncol=length(c(unlist(allNothing), always)), 
                                      nrow=nrow(restrictedsModels))
		      
              colnames(restrictedsMatrix)<-c(unlist(allNothing), always)
		    
		          ##Put in the configurations for the alwaysCondition variables into restrictedsMatrix.
		          restrictedsMatrix[,always]<-restrictedsModels[,"alwaysCondition"]

		          ##Do the same for the allNothingCondition. Since the allNothingCondition is a list, 
              ##each list should be considered separately.
	          
	            for(i in 1:length(allNothing)){
		            restrictedsMatrix[,allNothing[[i]]]<-restrictedsModels[,allNothingConditionNames[i]]
		          }
          
              ##cbind that to the expandgrid results for any of the "otherrestricteds" 
              ##which is just restricteds that are not always or allNothing types.
              ##They vary as normal in expand grid and are stripped out later.
              restrictedsMatrix<-cbind(restrictedsMatrix,restrictedsModels[,otherrestricteds])
             
              ##This strips out models that fail the eitherOr test. 
              ##Basically, it makes sure there is only one TRUE or all FALSES
              ##The as.logical line in there because, for some reason,
              ##it doesn't think the restrictedsMatrix is a logical.
              
              ##good models are TRUE
  		        eitherOrTest<-function(x){length(which(x==TRUE))==1 | any(as.logical(x))==FALSE}
              
              ##asks if the first value is FALSE. If it is, the model gets a TRUE
              ##if the first value is TRUE, then the model only gets a TRUE if everything is TRUE.
              conditionalsTest<-function(x){x[1]==FALSE | all(as.logical(x))==TRUE}
              
              ##Cannot apply over the whole row or it will apply to always and allNothing too
              ##so this indexes the matrix according only to those models in eitherOr
              ##The unlist part is to get it as a vector that can be used for indexing the whole matrix.
              eitherOrTestResults <- NULL
              for(i in 1:length(eitherOr)){
              	eitherOrStripIndex <- unlist(alply(restrictedsMatrix[eitherOr[[i]]], 1, eitherOrTest))
              	eitherOrTestResults <- c(eitherOrTestResults,eitherOrStripIndex)
              }
              
              conditionalsTestResults<-NULL
              for (i in 1:length(conditionals)){
                conditionalsStripIndex<-unlist(
                  alply(
                    ##This nasty piece is to bind together the conditionals with the variables
                    ##on which they're dependent. There should be a way to do this as a list
                    ##of lists instead, which would be more parsimonious. 
                    restrictedsMatrix[,cbind(conditionals[[i]], conditionedOnTheseVariables[[i]])],
                                                     1,conditionalsTest)##apply over rows the fun. conditionalsTest
                  )
                conditionalsTestResults<-c(conditionalsTestResults, conditionalsStripIndex)
              }
  		        
              ##Since eitherOrTestResults is a vector, transform it into a matrix with rows 
              ##indicating each set of the eitherOr condition and rows indicating model numbers. 
              ##The transformation into a matrix is for convenience in the next part. 
              eitherOrTestResults <- matrix(eitherOrTestResults,ncol=length(eitherOr),byrow=FALSE)
              
              conditionalsTestResults<-matrix(conditionalsTestResults, ncol=length(conditionals), byrow=FALSE)
              
              ##Given the matrix of eitherOrTestResults, create a vector whose element is 
              ##TRUE if all eitherOr conditions are TRUE. Otherwise, it is FALSE. This is 
              ##done because if the user specifies more than one eitherOr condition, models 
              ##for which the conditions hold for all eitherOr conditions should be calculated.
              TestResultsCombined <- aaply(cbind(eitherOrTestResults,conditionalsTestResults),1,
              function(x){
              	ifelse(all(x),TRUE,FALSE)
              })
              
              
              ##This indexes the matrix (and resaves it) by the test results .
              ##So if the test came back TRUE, the model is kept. If the test is false,
              ##it removes that row.
              restrictedsMatrix<-restrictedsMatrix[TestResultsCombined,]
              
              ######################End Tests################################################
          
          
              ##This function matches the models with a particular temp value to a row 
              ##in the restrictedsMatrix 
              ##Thus, for each model where temp==1 in unrestricteds, we match it to the first row
              ##of the restricteds. Same with 2 and so on all the way through the number of rows
              ##in the unrestricteds matrix (number of those type of models.
              bindTogether<-function(i){cbind(unrestrictedsMatrix[unrestrictedsMatrix$temp==i,], 
                                              restrictedsMatrix[i,])}
              
  		        ##This is an empty list for the unconditioned variables that will be put into the 
              ##expand.grid function.
  		        unrestrictedsList<-list()
  		    
  		    
  		        if(length(unrestricteds)!=0){
                
                ##The unrestrictedsList will not be created if all variables are conditioned. 
  		          ##If there are unconditioned variables, however, the following code generates a 
  		          ##list that says TRUE and FALSE for each unconditioned variable.
                length(unrestrictedsList)<-length(unrestricteds)
  		          unrestrictedsList<-llply(1:length(unrestricteds), 
  		                                    function(i){unrestrictedsList[[i]]<-c(TRUE, FALSE)},
  		                                    .parallel=parallel)
  		          unrestrictedsList<-c(unrestrictedsList, list(temp=1:nrow(restrictedsMatrix)))
                names(unrestrictedsList)<-c(unrestricteds, "temp")
                
                ##Expand grid on the unconditioned variables.
                unrestrictedsMatrix <- expand.grid(unrestrictedsList)
                
                ##Here, I'm llplying over all of the rows of the conditioned variable combination matrix
                ##This is because temp in the unrestricted matrix takes on a new value for each model 
                ##in the conditioned matrix.So this matches temp==1 in unconditioned to row 1 
                ##of the conditioned matrix 
                ##lply returns a list and laply doesn't work in this context, so I use do.call with rbind
                ##to get a matrix here.
                modelMatrix<-do.call("rbind",llply(1:nrow(restrictedsMatrix), bindTogether))
              
                ##Finally, we remove temp. This makes it so modelMatrix is exactly the same but without
                ##the temp variable.
                modelMatrix$temp<-NULL
              
                ##This ensures that the variables go back into the order they originally were in for the input
                ##matrix of fitbma. This is essential because they can easily get out of order in the
                ##conditioning process.
                modelMatrix<-modelMatrix[colnames(x)]
              
  		        }else{
  		        	modelMatrix <- restrictedsMatrix
  		        	
  		        	##This ensures that the variables go back into the order they originally were in for the input
                ##matrix of fitbma. This is essential because they can easily get out of order in the
                ##conditioning process.
                modelMatrix<-modelMatrix[colnames(x)]
  		        }

              return(modelMatrix)
            }##close modelSelect function

}##Close else condition for length(restricteds)=0 that gives function for modelMatrix when there are restrictions.

            ## This function runs the regressions for each combination
            ## i is a list of variable names contained in matrix x
            ## It takes each variable and standardizes them
            ## Runs a regression without a constant
            ## Outputs the relevant object of class lm
            run.regs<-function(i){
              thisLM <- lm(scale(y)~scale(x[,i])-1)
              thisCoef <- coef(thisLM);  names(thisCoef) <- colnames(x)[i]
              thisSE <-  summary(thisLM)$coefficients[,2]; names(thisSE) <-colnames(x)[i]
              thisR2 <- summary(thisLM)$r.squared
              return(list(coef=thisCoef, se=thisSE, R2=thisR2))
            }

            ##the bayesFactor function calculates the bayes factor 
            ##It takes an index
            ##Will add better comment later.
            bayesFactor<-function(index){
              bf <- (1+g)^((n-numberVars[index]-1)/2)*((1+g*(1-r2s[index]))^(-(n-1)/2))
              names(bf)<-c("bf")
              return(bf)
            }
                        
            ##Error thrown if non-unique column names.Each variable must have its own unique name.
            if(length(unique(colnames(x)))<ncol(x)){
              stop("Must have unique names for each column")
            }
 
			      ##Get the modelMatrix from the modelSelect function if there are restrictions.
			      if(length(restricteds)!=0){
			      modelMatrix <- modelSelect(varNames=colnames(x),
                                       always=always, 
                                       allNothing=allNothing, 
                                       eitherOr=eitherOr,
                                       parallel=parallel,
                                       conditionals=conditionals,
                                       conditionedOnTheseVariables=conditionedOnTheseVariables)
		  
		  ##Convert modelMatrix, which is actually a data frame, to a matrix.
			      modelMatrix <- as.matrix(modelMatrix)
			      
		  }##Close if condition for length(restricteds)!=0 to get modelMatrix.

			      ##Get the list of lm objects associated with all possible regressions
            lmList<-alply(modelMatrix,1,run.regs, .parallel=parallel)

            ##This gets the r.squared values and puts them in a list
            r2s<-laply(lmList, function(x){return(x[["R2"]])}, .parallel=parallel)

            ##Extracts the coefficients and ses
            coefs<-llply(lmList, function(x){return(x[["coef"]])}, .parallel=parallel)
            standardErrors<-llply(lmList, function(x){return(x[["se"]])}, .parallel=parallel)

            ## Calculate the number of variabels in each model
            numberVars <- rowSums(modelMatrix)

            ## Some useful constants
            n <- length(y)
            m <- nrow(modelMatrix) # number of total models
            p <- ncol(modelMatrix)+1
            
            ##vector of bmk values for each model
            bfVec<-aaply(1:m,.margins=1,.fun=bayesFactor, .parallel=parallel)

            ## Posterior probability of each model
            postProb <- matrix(bfVec/sum(bfVec), ncol=1)

            ##Pr(B!=0)
            postProbcoefs <- t(modelMatrix)%*%postProb

            ##Get the names of the covariates whose coefficients are estimated for each model. 
            coefMatrix <- sdMatrix <- modelMatrix
            colnames(coefMatrix) <- colnames(sdMatrix) <- colnames(modelMatrix)
            sdMatrix[modelMatrix==FALSE] <- coefMatrix[modelMatrix==FALSE] <- 0

            ## Make the coefficients list into a matrix
            for(i in 1:m){
              coefMatrix[i,names(coefs[[i]])] <- coefs[[i]]
            }

            for(i in 1:m){
              sdMatrix[i,names(standardErrors[[i]])] <- standardErrors[[i]]
            }
            
            ## E(B)
            expB <- t(coefMatrix)%*%postProb

            ## E(B|B!=0)
            expBcond <- rep(NA, p-1)
            for(i in 1:(p-1)){
              these <- (modelMatrix[,i]==TRUE)
              expBcond[i] <- (coefMatrix[these,i])%*% (postProb[these,])
            }

            ## se(B|B!=0)
            condSE <- rep(NA, p-1)
            for(i in 1:(p-1)){
              these <- (modelMatrix[,i]==TRUE)
              condSE[i] <- (sdMatrix[these,i])^2%*% (postProb[these,])
            }
            condSE <- sqrt(condSE)

            ## Pr(B>0|B!=0)
            largerZero <- rep(NA, p-1)
            for(i in 1:(p-1)){
              these <- (modelMatrix[,i]==TRUE)
              largerZero[i] <- pnorm(0, coefMatrix[these,i], sdMatrix[these,i], lower.tail=FALSE) %*%(postProb[these,])
             }
             
            ##Returned everything.
            ##The documentation will need to be changed for the help files.
            return(new("bma", 
                       x=x, 
                       y=y, 
                       coefs=coefs,
                       standardErrors=standardErrors,
                       r2s=r2s,
                       postProb=postProb,
                       postProbcoefs=postProbcoefs,
                       bfVec=bfVec,
                       expB=expB,
                       expBcond=expBcond,
                       largerZero=largerZero,
                       condSE=condSE,
                       coefMatrix=coefMatrix,
                       sdMatrix=sdMatrix)
            )##close return on function

          }#close function definition
          ) ##Close method



