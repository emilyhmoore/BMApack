#' plot.bma Function
#'
#' Plots bma objects as returned by fitBMA.
#' Plots the density of the normal distribution centered at expected coefficient and with a standard 
#' deviation equal to the weighted standard error of variable.
#' Also plots line segment centered at zero indicating probability that coefficient is zero.
#' Will need to scroll through the plots, each corresponding to a particular variable.
#'
#' @author Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily Moore
#' @examples
#' 
#' data<-matrix(rnorm(1000), ncol=10)
#' colnames(data)<-c(paste("x", 1:10, sep=""))
#' datay<-5*data[,2]+3*data[,3]+rnorm(100)
#' trial<-(fitBMA(x=data, y=datay, g=3, parallel=FALSE)) 
#' plot(trial)
#' @rdname plot.bma
#' @export
#' 
setMethod(f="plot", signature="bma",
          definition=function(x,y,...){
            devAskNewPage(TRUE) #Setting so that you can scroll through various plots.
            
            BMAtheses <- x@standardErrors #Extracting the standard deviations.
            
            BMAtheses[is.na(BMAtheses)] <- 0 #Setting NAs to 0.
            
            BMAthecoefs <- x@coefs #Extracting the coefficients.
            
            BMAthecoefs[is.na(BMAthecoefs)] <- 0 #Setting NAs to 0.

            #Creating a list of coefficient estimates for all variables accross all models.
            #Each element in the list represents the coefficient estimates for a particular
            #variable for each model.
            ceofslist.fun <- function(i){
              coefslist <- list(NULL)
              coefslist <- list(matrix(BMAthecoefs[,i]))
              return(coefslist)
            }
            coefslist <- aaply(1:ncol(BMAthecoefs), .margins=1, .fun=ceofslist.fun)
            
            #Creating a list of coefficient estimates for all variables across all models.
            #Each element in the list represents the coefficient estimates for a particular
            #variable for each model.
            seslist.fun <- function(i){
              seslist <- list(NULL)
              seslist <- list(matrix(BMAtheses[,i]))
              return(seslist)
            }
            seslist <- aaply(1:ncol(BMAtheses), .margins=1, .fun=seslist.fun)
            
            #Extracting model odds.
            modelodds <- x@bmk
            
            #Combining the coefficient estimates, standard deviations, and model odds for each
            #variable. Each element in the list represents one variable.
            cbind.fun <- function(i){
              cbindmat <- list(NULL)
              cbindmat <- cbind(coefslist[[i]], seslist[[i]], modelodds)
              return(cbindmat)
            }
            totalmat <- llply(1:length(coefslist), .fun=cbind.fun)
            
            #Omitting those models for which the specific variable is not included.
            zeroelim.fun <- function(i){
              nonzeromat <- list(NULL)
              nonzeromat <- totalmat[[i]][rowSums(totalmat[[i]]==0)==0,]
              return(nonzeromat)
            }
            nonzeromat <- llply (1:length(totalmat), .fun=zeroelim.fun)
            
            #Running dnorm function across all variables, using appropriate coefficient estimate
            #and standard deviation.
            dnorm.fun <- function(j){
              dnorm.fun2 <- function(i){
                dnormmat <- matrix()
                dnormmat <- dnorm(seq(min(nonzeromat[[j]][,1])-4*max(nonzeromat[[j]][,2]), 
                                      
                                      max(nonzeromat[[j]][,1])+4*max(nonzeromat[[j]][,2]), length=100),  
                                  
                                  mean=nonzeromat[[j]][i,1], sd=nonzeromat[[j]][i,2])
                return(dnormmat)
              }
              dnormmat <- t(aaply(1:nrow(nonzeromat[[j]]), .fun=dnorm.fun2, .margins=1))
            }
            dnormmat2 <- llply(1:length(nonzeromat), .fun=dnorm.fun)
            
            #Calculating the height of y at all points in sequence, which will be used to plot
            #the posterior distributions.
            yheight.fun <- function(i){
              heightmat <- list()
              heightmat <- dnormmat2[[i]] %*% matrix(nonzeromat[[i]][,3])
              return(heightmat)
            }
            heightmat <- llply(1:length(dnormmat2), .fun=yheight.fun)
            
            #Calculating probability that particular variable is not included in model. Will be 
            #used for vertical bar centered at zero on graph.
            exclusion.fun <- function(i){
              exclusion <- numeric()
              exclusion <- sum(is.na(x@theses[,i]))/length(x@theses[,i])
              return(exclusion)
            }
            exclusion <- aaply(1:ncol(x@theses), .margins=1, .fun=exclusion.fun)
            
            #Plotting posterior distributions of variables. Setting the x-axis to span 
            #4 standard deviations to left and right of coefficient estimate. Adding
            #vertical bar to indicate probability that variable is not included in model.
            coef.plot <- function(i){
              plot(seq(min(nonzeromat[[i]][,1])-4*max(nonzeromat[[i]][,2]), 
                       
                       max(nonzeromat[[i]][,1])+4*max(nonzeromat[[i]][,2]), length=100),
                   
                   heightmat[[i]], type="l", xlab="", ylab="", main=paste("Variable", i))
              
              segments(0,0,0,exclusion[i], lwd=3)       
            }
            
            l_ply(1:length(heightmat), coef.plot)
          })