#' fitBMA Function
#'
#' Runs regression on all possible combos of covariates and returns coefs, R2, and BMA stats
#'
#' @param x: A numeric matrix of covariates
#' @param y: A numeric vector of the same length as the number of rows in x.
#' @param g: A value for g. 
#' @param parallel: runs in parallel if TRUE
#'
#' @return An bma class object with following slots: 
#'  \item{combo.coef}{A list of coefficients from each regression}
#'  \item{combo.fit}{Vector of R-squared Values} 
#'  \item{bmk}{Vector of posterior probability odds}
#'  \item{exp.vals}{A vector of expected coefficient values}
#'  \item{coefprobs}{A vector of probabilities that the coefficient is non-zero}
#' @author Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily Moore
#' @examples
#' 
#' x1<-rnorm(500)
#' x2<-rnorm(500,3,15)
#' dep<-(x1+2*x2)+rnorm(500,4,100)
#' covars<-cbind(x1,x2) 
#' fitBMA(x=covars, y=dep, parallel=FALSE)
#' @rdname fitBMA
#' @export

setGeneric(name="fitBMA",
           def=function(x, y, g=3, parallel=TRUE,allNothing=NULL, eitherOr=NULL,always=NULL)
           {standardGeneric("fitBMA")}
           ) 


setMethod(f="fitBMA",
          definition=function(x, y, g=3, 
                              parallel=FALSE,
                              allNothing=NULL, 
                              eitherOr=NULL,
                              always=NULL
                              )
        {
          
            ##Extract the names of the independent variables, which will be used in later functions.
            varNames <- colnames(x)

            ##The modelSelect function returns the correct model configurations.  
            modelSelect<-function(varNames, 
                                    parallel,
                                    allNothing, 
                                    eitherOr,
                                    always
                                    )
              { 
		          ##Throw errors if the conditions specified are inappropriate.
  		        if(length(allNothing)==1){stop("If specifying allNothing, 
                                             it must have at least two variables")}
  		        if(length(eitherOr)==1){stop("If specifying eitherOr, 
                                           it must have at least two variables")}
          
  		        ##The conditionals object contains variables that are conditioned.
  		        conditionals<-c(allNothing,always,eitherOr)
  		  
  		        ##conditionalsIndex returns the index of the conditioned variables. 
              ##This is necessary because we want to separate the conditioned ones 
              ##from the unconditioned variables.
  		        conditionalsIndex <- which(varNames%in%conditionals)
  		        unconditionals <- varNames[-conditionalsIndex]

		          ##The always condition is always included.
		          alwaysCondition <- TRUE
		  
		          ##The allNothing condition is either included or not included.
		          allNothingCondition<-c(TRUE, FALSE)
		          ##
              ##We will need to be able to take lists of allnothings eventually.
              ##
		          ##The conditionalsList is all configurations for the conditioned variables combined.
		          conditionalsList<-list(alwaysCondition=alwaysCondition, 
                                    allNothingCondition=allNothingCondition)
          
              ##This is all conditionals that are not always or allNothing types
  		        otherConditionals<-conditionals[-c(which(conditionals%in%always), 
                                                which(conditionals%in%allNothing))]
          
              ##Make a list for the variables that are just going to be true false before they are 
              ##stripped away
              otherConditionalsList<-list()
		          otherConditionalsList<-llply(1:length(otherConditionals),
                                          function(i){otherConditionalsList[[i]]<-c(TRUE, FALSE)},
                                          .parallel=parallel)
		          names(otherConditionalsList)<-otherConditionals
          
              conditionalsList<-c(conditionalsList, otherConditionalsList)

              ##Expand grid on the conditioned variables.
              conditionalsModels <- expand.grid(conditionalsList)

              conditionalsMatrix <-matrix(rep(FALSE),ncol=length(c(allNothing, always)), 
                                      nrow=nrow(conditionalsModels))
		      
              colnames(conditionalsMatrix)<-c(allNothing, always)
		    
		          ##Put in the configurations for the alwaysCondition variables into conditionalsMatrix.
		          conditionalsMatrix[,always]<-conditionalsModels[,"alwaysCondition"]

		          ##Do the same for the allNothingCondition.This should look the same 
              ##for each variable in a set of allNothings 
		          conditionalsMatrix[,allNothing]<-conditionalsModels[,"allNothingCondition"]
          
              ##cbind that to the expandgrid results for any of the "otherConditionals" 
              ##which is just conditionals that are not always or allNothing types.
              ##They vary as normal in expand grid and are stripped out later.
              conditionalsMatrix<-cbind(conditionalsMatrix,conditionalsModels[,otherConditionals])
        
              ###############################################################################
              ###############This is where we need to strip out "bad" models#################
              ###############BEFORE the temp variable is created for use with################
              ###############the unconditionals##############################################
              ###############################################################################
          
  		        ##This is an empty list for the unconditioned variables that will be put into the expand.grid function.
  		        unconditionalsList<-list()
  		    
  		        ##The unconditionalsList will not be created if all variables are conditioned. 
  		        ##If there are unconditioned variables, however, the following code generates a 
  		        ##list that says TRUE and FALSE for each unconditioned variable.
  		        if(length(unconditionals)!=0){
                length(unconditionalsList)<-length(unconditionals)
  		          unconditionalsList<-llply(1:length(unconditionals), 
  		                                    function(i){unconditionalsList[[i]]<-c(TRUE, FALSE)},
  		                                    .parallel=parallel)
  		          unconditionalsList<-c(unconditionalsList, list(temp=1:nrow(conditionalsMatrix)))
                names(unconditionalsList)<-c(unconditionals, "temp")
  		        }
  
  		        ##Expand grid on the unconditioned variables.
  		        unconditionalsMatrix <- expand.grid(unconditionalsList)
          
              ##This function matches the models with a particular temp value to a row 
              ##in the conditionalsMatrix 
              ##Thus, for each model where temp==1 in unconditionals, we match it to the first row
              ##of the conditionals. Same with 2 and so on all the way through the number of rows
              ##in the unconditionals matrix (number of those type of models.
              bindTogether<-function(i){cbind(unconditionalsMatrix[unconditionalsMatrix$temp==i,], conditionalsMatrix[i,])}
          
              ##Here, I'm llplying over all of the rows of the conditioned variable combination matrix
              ##This is because temp in the unconditional matrix takes on a new value for each model 
              ##in the conditioned matrix.So this matches temp==1 in unconditioned to row 1 
              ##of the conditioned matrix 
              ##lply returns a list and laply doesn't work in this context, so I use do.call with rbind
              ##to get a matrix here.
              modelMatrix<-do.call("rbind",llply(1:nrow(conditionalsMatrix), bindTogether))
          
              ##Finally, we remove temp. This makes it so modelMatrix is exactly the same but without
              ##the temp variable.
              modelMatrix$temp<-NULL
          
              ##This ensures that the variables go back into the order they originally were in for the input
              ##matrix of fitbma. This is essential because they can easily get out of order in the
              ##conditioning process.
              modelMatrix<-modelMatrix[colnames(x)]
          
              return(modelMatrix)
            }##close modelSelect function
        
       
            ##Trying out modelSelect.
            ##modelSelect(varNames=varNames,
                        ##allNothing=c("var 1", "var 2"), 
                        ##eitherOr=c("var 3", "var 4"), 
                        ##always="var 5", 
                        ##parallel=FALSE
                        ##)

            ## This function runs the regressions for each combination
            ## i is a list of variable names contained in matrix x
            ## It takes each variable and standardizes them
            ## Runs a regression without a constant
            ## Outputs the relevant object of class lm
            run.regs<-function(i){
              thisLM <- lm(y~x[,i])
              thisCoef <- coef(thisLM);  names(thisCoef) <- c("(Intercept)", colnames(x)[i])
              thisSE <-  summary(thisLM)$coefficients[,2]; names(thisSE) <- c("(Intercept)", colnames(x)[i])
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

#####JACOB'S CODE THAT CONFIGURES MODEL COMBINATIONS WITHOUT ANY CONDITIONS SPECIFIED ARE COMMENTED OUT STARTING HERE#####           
            ##Make the set of all possible combinations of variables
            ##Returns a list for now.
            #varList <- list()  ## an empty list
            #for(i in colnames(x)){
              #varList <- c(varList,list(i=c(FALSE,TRUE)))
            #}
            #names(varList) <- colnames(x)

            ##varList is now a list with labels the same as 'x' containing FALSE and TRUE for each variables
            #modelMatrix <- as.matrix(expand.grid(varList, KEEP.OUT.ATTRS=FALSE)) ## The complete list of all possible models
            #modelMatrix <- modelMatrix[-1, ] # Remove the null model here

            ##Get the list of lm objects associated with all possible regressions
            #lmList<-alply(modelMatrix,1, run.regs, .parallel=parallel)
#####JACOB'S CODE THAT CONFIGURES MODEL COMBINATIONS WITHOUT ANY CONDITIONS SPECIFIED ARE COMMENTED OUT ENDING HERE#####
 
			      ##Get the modelMatrix from the modelSelect function.
			      modelMatrix <- modelSelect(varNames=colnames(x),
                                       always=always, 
                                       allNothing=allNothing, 
                                       eitherOr=eitherOr,
                                       parallel=parallel)
		  
			      ##Convert modelMatrix, which is actually a data frame, to a matrix.
			      modelMatrix <- as.matrix(modelMatrix)

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
            head(r2s)

            ## Posterior probability of each model
            postProb <- matrix(bfVec/sum(bfVec), ncol=1)

            ##Pr(B!=0)
            postProbcoefs <- t(modelMatrix)%*%postProb

            ##Get the names of the covariates whose coefficients are estimated for each model. 
            coefMatrix <- sdMatrix <- cbind(rep(NA, m), modelMatrix)
            colnames(coefMatrix) <- colnames(sdMatrix) <- c("(Intercept)", colnames(modelMatrix))
            sdMatrix[modelMatrix==FALSE] <- coefMatrix[modelMatrix==FALSE] <- 0

            ## Make the coefficients list into a matrix
            for(i in 1:m){
              coefMatrix[i,names(coefs[[i]])] <- coefs[[i]]
            }

            for(i in 1:m){
              sdMatrix[i,names(standardErrors[[i]])] <- standardErrors[[i]]
            }


            ## model space with constant
            modelMatrixConst <- cbind(TRUE, modelMatrix)
            colnames(modelMatrixConst) <- c("(Intercept)", colnames(modelMatrix))
            
            ## E(B)
            expB <- t(coefMatrix)%*%postProb

            ## E(B|B!=0)
            expBcond <- rep(NA, p)
            for(i in 1:p){
              these <- (modelMatrixConst[,i]==TRUE)
              expBcond[i] <- (coefMatrix[these,i])%*% (postProb[these,])
            }

            ## se(B|B!=0)
            condSE <- rep(NA, p)
            for(i in 1:p){
              these <- (modelMatrixConst[,i]==TRUE)
              condSE[i] <- (sdMatrix[these,i])^2%*% (postProb[these,])
            }
            condSE <- sqrt(condSE)

            ## Pr(B>0|B!=0)
            largerZero <- rep(NA, p)
            for(i in 1:p){
              these <- (modelMatrixConst[,i]==TRUE)
              largerZero[i] <- pnorm(0, coefMatrix[these,i], sdMatrix[these,i], lower.tail=FALSE) %*%(postProb[these,])
             }
             
            ##Returned everything basically.
            ##The documentation will need to be changed for the help files.
            ##I think I got them all. Let me know if I missed any. 
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

x=matrix(rnorm(1000), ncol=10)
colnames(x)<-paste("var", 1:10)
y<-5*x[,1]+2*x[,2]+rnorm(100)
trial<-fitBMA(x=x,y=y,allNothing=c("var1", "var2"), always="var 3", eitherOr=c("var 4", "var 5"))

