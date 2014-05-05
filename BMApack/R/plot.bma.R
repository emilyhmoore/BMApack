#' plot.bma Function
#' 
#' Taking an object of class bma as input, the function generates posterior coefficient plots 
#' for each variable included in the analysis. The area under the curve over a particular 
#' interval on the x-axis represents the probability that the coefficient assumes a value along 
#' that interval. The vertical line positioned at zero represents the probability that the 
#' variable is not included in a model. The user will need to scroll through the various plots
#' that are generated, each corresponding to a particular variable.
#'
#' @author Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily Moore
#' @examples
#' 
#' x <- matrix(rnorm(220), ncol=11)
#' colnames(x) <- paste("X", 1:11,sep="")
#' y <- rnorm(20)
#' allNothing <- list(c("X1","X2"))
#' eitherOr <- list(c("X3", "X4"))
#' always <- "X5"
#' squaredInteraction <- list(c("X6", "X7"))
#' constituentTerms <- list(c("X8","X9"))
#' BMAObject <- fitBMA(x, y, g=3, parallel=FALSE, allNothing, eitherOr, always,squaredInteraction,constituentTerms)
#' plot(BMAObject)
#' 
#' @rdname plot.bma
#' @aliases plot.bma,ANY-method plot.bma
#' @export
#' 
setMethod(f="plot", signature="bma",
          definition=function(x,y,...){
            devAskNewPage(TRUE) #Setting so that you can scroll through various plots.
            
            standardErrors <- x@sdMatrix #Extracting the standard errors matrix
            
            modelCoefficients <- x@coefMatrix #Extracting the coefficients matrix
            
            
            #Creating a list of coefficient estimates for all variables accross all models.
            #Each element in the list represents the coefficient estimates for a particular
            #variable for each model.
            ceofList.fun <- function(i){
              coefList <- list(NULL)
              coefList <- list(matrix(modelCoefficients[,i]))
              return(coefList)
            }
            coefList <- aaply(1:ncol(modelCoefficients), .margins=1, .fun=ceofList.fun)
            
            
            #Creating a list of standard errors for all variables across all models.
            #Each element in the list represents the standard error for a particular
            #variable for each model.
            standardErrorsList.fun <- function(i){
              sdList <- list(NULL)
              sdList <- list(matrix(standardErrors[,i]))
              return(sdList)
            }
            standardErrorsList<- aaply(1:ncol(standardErrors), .margins=1, .fun=standardErrorsList.fun)
            
            
            #Extracting model odds.
            modelOdds <- x@postProb
            
            
            #Combining the coefficient estimates, standard errors, and model odds for each
            #variable. Each element in the list represents one variable.
            cbind.fun <- function(i){
              cbindMatrix <- list(NULL)
              cbindMatrix <- cbind(coefList[[i]], standardErrorsList[[i]], modelOdds)
              return(cbindMatrix)
            }
            totalMatrix <- llply(1:length(coefList), .fun=cbind.fun)
            
            #Omitting those models for which the specific variable is not included.
            zeroEliminator.fun <- function(i){
              nonZeroMatrix <- list(NULL)
              nonZeroMatrix <- totalMatrix[[i]][rowSums(totalMatrix[[i]]==0)==0,]
              return(nonZeroMatrix)
            }
            nonZeroMatrix <- llply (1:length(totalMatrix), .fun=zeroEliminator.fun)
            
            #This for loop crates an empty list where the values from running the dnorm function over the models will
            #eventually be inserted.
            dnormMatrix <- list()
            for(j in 1:length(nonZeroMatrix)){
              dnormMatrix[[j]] <- matrix(ncol=nrow(nonZeroMatrix[[j]]), nrow=1000)
            }            
            
            #Running dnorm function across all variables, using appropriate coefficient estimate
            #and standard deviation.
            for(i in 1:length(dnormMatrix)){
              for(j in 1:nrow(nonZeroMatrix[[i]])){
                dnormMatrix[[i]][,j] <- dnorm(seq(min(nonZeroMatrix[[i]][,1])-3*max(nonZeroMatrix[[i]][,2]), 
                                                  max(nonZeroMatrix[[i]][,1])+3*max(nonZeroMatrix[[i]][,2]), length=1000), mean=nonZeroMatrix[[i]][j],
                                              sd=nonZeroMatrix[[i]][,2][j])
              }
            }
            
            
            #Calculating the height of y at all points in sequence, which will be used to plot
            #the posterior distributions.
            yHeight.fun <- function(i){
              heightMatrix <- list()
              heightMatrix <- dnormMatrix[[i]] %*% matrix(nonZeroMatrix[[i]][,3])
              return(heightMatrix)
            }
            heightMatrix <- llply(1:length(dnormMatrix), .fun=yHeight.fun)
            
            #Calculating probability that particular variable is not included in model. Will be 
            #used for vertical bar centered at zero on graph.            
            exclusion.fun <- function(i){
              exclusion <- numeric()
              exclusion <- length(which(standardErrors[,i] == 0))/length(standardErrors[,i])
              return(exclusion)
            }
            exclusion <- aaply(1:ncol(standardErrors), .margins=1, .fun=exclusion.fun)
            
            
            
            #Plotting posterior distributions of variables. Setting the x-axis to span 
            #3 standard deviations to left and right of coefficient estimate. Adding
            #vertical bar to indicate probability that variable is not included in model.
            coef.plot <- function(i){
              plot(seq(min(nonZeroMatrix[[i]][,1])-3*max(nonZeroMatrix[[i]][,2]), 
                       
                       max(nonZeroMatrix[[i]][,1])+3*max(nonZeroMatrix[[i]][,2]), length=1000),
                   
                   heightMatrix[[i]], type="l", xlab="", ylab="", main=colnames(x@x)[i],
                   
                   ylim=c(0, max(exclusion[i], max(heightMatrix[[i]]))))
              
              segments(0,0,0,exclusion[i], lwd=3)
            }
            
            l_ply(1:length(heightMatrix), coef.plot)
          })


