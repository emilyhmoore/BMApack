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
           def=function(x, y, g=3, parallel=TRUE,core=100,...)
           {standardGeneric("fitBMA")}
           )

setMethod(f="fitBMA",
          definition=function(x, y, g=3, parallel=TRUE)
          {
            
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

            ##function is intended to be used to aaply over the rows of the matrix above ## and this is not a good comment.
            ## aplly what over the rows, to achieve what?
            ## This is not a good name for a function
            bayesFactor<-function(index){
              bf <- (1+g)^((n-numberVars[index]-1)/2)*((1+g*(1-r2s[index]))^(-(n-1)/2))
              names(bf)<-c("bf")
              return(bf)
            }
                        
            ##Error thrown if non-unque column names.
            if(length(unique(colnames(x)))<ncol(x)){
              stop("Must have unique names for each column")
            }
            
            ##Make the set of all possible combinations of variables
            varList <- list()  ## an empty list
            for(i in colnames(x)){
              varList <- c(varList,list(i=c(FALSE,TRUE)))
            }
            names(varList) <- colnames(x)

            ##varList is now a list with labels the same as 'x' containing FALSE and TRUE for each variables
            modelSpace <- as.matrix(expand.grid(varList, KEEP.OUT.ATTRS=FALSE)) ## The complete list of all possible models
            modelSpace <- modelSpace[-1, ] # Remove the null model here

            ##Get the list of lm objects associated with all possible regressions
            lmList<-alply(modelSpace,1, run.regs, .parallel=parallel)

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




