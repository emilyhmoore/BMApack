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
           def=function(x, y, g=3, parallel=TRUE,allNothing=NULL, eitherOr=NULL,always=NULL,interactions=NULL)
           {standardGeneric("fitBMA")}
           ) 


setMethod(f="fitBMA",
          definition=function(x, y, g=3, parallel=TRUE,allNothing=NULL, eitherOr=NULL,always=NULL,interactions=NULL)
          {
          
          ##Extract the names of the independent variables, which will be used in later functions.
          varNames <- colnames(x)
          
          ##The modelSelect function returns the correct model configurations.  
          modelSelect<-function(varNames=varNames, parallel=parallel,allNothing=allNothing, eitherOr=eitherOr,always=always,interactions=interactions
)
{ 
	
		  ##Throw errors if the conditions specified are inappropriate.
  		  if(length(allNothing)==1){stop("If specifying allNothing, it must have at least two variables")}
  		  if(length(eitherOr)==1){stop("If specifying eitherOr, it must have at least two variables")}
  		  if(length(interactions) < 2){stop("If specifying interaction, it must have at least two variables")}
  		  
  		  ##The conditionals object contains variables that are conditioned.
  		  conditionals<-c(allNothing, eitherOr,always,interactions)
  		  
  		  ##conditionalsIndex returns the index of the conditioned variables. This is necessary because we want to separate the conditioned ones from the unconditioned variables.
  		  conditionalsIndex <- which(varNames%in%conditionals)
  		  unconditionals <- varNames[-conditionalsIndex]
  		  
  		  ##This is an empty list for the unconditioned variables that will be put into the expand.grid function.
  		  unconditionalsList<-list()
  		  
  		  ##The unconditionalsList will not be created if all variables are conditioned. If there are unconditioned variables, however, the following code generates a list that says TRUE and FALSE for each unconditioned variable.
  		  if(length(unconditionals)!=0){
    length(unconditionalsList)<-length(unconditionals)
    unconditionalsList<-llply(1:length(unconditionals), function(i){unconditionalsList[[i]]<-c(TRUE, FALSE)},.parallel=parallel)
    names(unconditionalsList)<-unconditionals
  }

		  ##The always condition is always included.
		  alwaysCondition <- TRUE
		  
		  ##The allNothing condition is either included or not included.
		  allNothingCondition<-c(TRUE, FALSE)
		  
		  ##For the eitherOr condition, only one variable is included or none are included. 
		  eitherOrCondition <- c(eitherOr, FALSE)
		  
		  ##"TRUE" indicates the case when the interaction term is included along with the constituent terms. When either of the variables in the interactions object is specified, it means that only that variable is included. If "both", both constituent terms are included, but not the interaction term. If "neither", none of the variables are included.
		  interactionsCondition<-c(TRUE, interactions[1:length(interactions)], "both", "neither")
		  
		  ##The conditionalsList is all configurations for the conditioned variables combined.
		  conditionalsList<-list(alwaysCondition=alwaysCondition, allNothingCondition=allNothingCondition, eitherOrCondition=eitherOrCondition,interactionsCondition=interactionsCondition
  )

	      ##Merge conditionalsList and the unconditionalsList.
  configurationsList<-c(conditionalsList, unconditionalsList)

		  ##Use the expand.grid function to calculate all model configurations.  
		  modelConfigurations <- expand.grid(configurationsList)
		  
		  ##modelMatrix is an empty matrix with the independent variables in the columns and all model configurations expanded out in the rows. Note that the matrix has an additional column for the newly created interaction term.
		  modelMatrix <-matrix(rep(0),ncol=length(varNames)+1, nrow=nrow(modelConfigurations))
		  colnames(modelMatrix)<- c(varNames,"interaction")
		  
		  ##Convert the matrix into a data frame for compatibility with the expand.grid function.
		  modelMatrix <- as.data.frame(modelMatrix)
		  
		  ##Put in the configurations for the unconditioned variables (which are indicated by TRUE or FALSE) into modelMatrix.
		  modelMatrix[,unconditionals]<-modelConfigurations[,unconditionals]
		  
		  ##Put in the configurations for the alwaysCondition variables into modelMatrix.
		  modelMatrix[,always]<-modelConfigurations[,"alwaysCondition"]
		  
		  ##Do the same for the allNothingCondition and eitherOrCondition variables. 
		  modelMatrix[,allNothing]<-modelConfigurations[,"allNothingCondition"]
		  
		  for (i in 1:length(eitherOr)){modelMatrix[,eitherOr[i]]<-modelConfigurations[,"eitherOrCondition"]==eitherOr[i]}
		  
		  ##This for loop assigns TRUE to all variables in the interactions object to represent cases in which the interaction term and the constituent terms are included.
		  for(i in 1:length(interactions)){
  	modelMatrix[which(modelConfigurations[,"interactionsCondition"]==TRUE),interactions[i]] <- TRUE
  	
  }

		 ##For rows in which the constitutent terms are included, the interaction term should be included as well.
modelMatrix[which(modelMatrix[,interactions[1]]==1),"interaction"]<-TRUE
  
	 	  ##This for loop assigns TRUE to each constituent term to represent cases in which a single variable is included in the model.
	 	  for(i in 1:(length(interactions))){
  	modelMatrix[which(modelConfigurations[,"interactionsCondition"]%in%interactions[i]),interactions[i]] <- TRUE
  }
  
  		  ##This for loop assigns TRUE to both constituent terms to represent cases in which there is no interaction, but the constituent terms are included in the model.
  		  for(i in 1:(length(interactions))){
  	modelMatrix[which(modelConfigurations[,"interactionsCondition"]=="both"),interactions[i]] <- TRUE
  }
  
  		  ##This for loop assigns FALSE to all variables in the interactions object to represent cases in which none of the variables are included in the model.
  		  for(i in 1:(length(interactions))){
  	modelMatrix[which(modelConfigurations[,"interactionsCondition"]=="neither"),interactions[i]] <- FALSE
  }	
  	  
  	      return(modelMatrix)
}

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

            ##the bayesFactor function function calculates the bayes factor 
            ##It takes an index
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
            #modelSpace <- as.matrix(expand.grid(varList, KEEP.OUT.ATTRS=FALSE)) ## The complete list of all possible models
            #modelSpace <- modelSpace[-1, ] # Remove the null model here

            ##Get the list of lm objects associated with all possible regressions
            #lmList<-alply(modelSpace,1, run.regs, .parallel=parallel)
#####JACOB'S CODE THAT CONFIGURES MODEL COMBINATIONS WITHOUT ANY CONDITIONS SPECIFIED ARE COMMENTED OUT ENDING HERE#####
 
			##Get the modelMatrix from the modelSelect function.
			modelMatrix <- modelSelect()
		  
			##Convert modelMatrix, which is actually a data frame, to a matrix.
			modelMatrix <- as.matrix(modelMatrix)
			
			##Get the list of lm objects associated with all possible regressions
            lmList<-alply(modelMatrix,1, run.regs, .parallel=parallel)
            
            ##This gets the r.squared values and puts them in a list
            r2s<-laply(lmList, function(x){return(x[["R2"]])}, .parallel=parallel)

            ##Extracts the coefficients and ses
            coefs<-llply(lmList, function(x){return(x[["coef"]])}, .parallel=parallel)
            standardErrors<-llply(lmList, function(x){return(x[["se"]])}, .parallel=parallel)

            ## Calculate the number of variabels in each model
            numberVars <- rowSums(modelSpace)
            ## Some useful constants
            n <- length(y)
            m <- nrow(modelSpace) # number of total models
            p <- ncol(modelSpace)+1
            
            ##vector of bmk values for each model
            bfVec<-aaply(1:m,.margins=1,.fun=bayesFactor, .parallel=parallel)
            head(r2s)

            ## Posterior probability of each model
            postProb <- matrix(bfVec/sum(bfVec), ncol=1)

            ##Pr(B!=0)
            postProbcoefs <- t(modelSpace)%*%postProb

            ##Get the names of the covariates whose coefficients are estimated for each model. 
            coefMatrix <- sdMatrix <- cbind(rep(NA, m), modelSpace)
            colnames(coefMatrix) <- colnames(sdMatrix) <- c("(Intercept)", colnames(modelSpace))
            sdMatrix[modelSpace==FALSE] <- coefMatrix[modelSpace==FALSE] <- 0

            ## Make the coefficients list into a matrix
            for(i in 1:m){
              coefMatrix[i,names(coefs[[i]])] <- coefs[[i]]
            }

            for(i in 1:m){
              sdMatrix[i,names(standardErrors[[i]])] <- standardErrors[[i]]
            }


            ## model space with constant
            modelSpaceConst <- cbind(TRUE, modelSpace)
           colnames(modelSpaceConst) <- c("(Intercept)", colnames(modelSpace))
            
            ## E(B)
            expB <- t(coefMatrix)%*%postProb

            ## E(B|B!=0)
            expBcond <- rep(NA, p)
            for(i in 1:p){
              these <- (modelSpaceConst[,i]==TRUE)
              expBcond[i] <- (coefMatrix[these,i])%*% (postProb[these,])
            }

            ## se(B|B!=0)
            condSE <- rep(NA, p)
            for(i in 1:p){
              these <- (modelSpaceConst[,i]==TRUE)
              condSE[i] <- (sdMatrix[these,i])^2%*% (postProb[these,])
            }
            condSE <- sqrt(condSE)

            ## Pr(B>0|B!=0)
            largerZero <- rep(NA, p)
            for(i in 1:p){
              these <- (modelSpaceConst[,i]==TRUE)
              largerZero[i] <- pnorm(0, coefMatrix[these,i], sdMatrix[these,i], lower.tail=FALSE) %*%(postProb[these,])
             }

            ## The returned object needs to be changed arond
            return(new("bma", x=x, y=y, thecoefs=thecoefs,
                       combo.coef=coefs,
                       theses=theses,
                       combo.fit=fits,bmk=odds.bmk,
                       exp.vals=exp.val,
                       coefprobs=coefprob,
                       coefprobs.largerthanzero=coefprob.largerthanzero,
                       conditional.sds=conditional.sd))

          }#close function definition
          ) ##Close method




