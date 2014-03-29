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
            devAskNewPage(TRUE)
            exclusion.fun <- function(i){
              exclusion <- numeric()
              exclusion <- sum(is.na(x@theses[,i]))/length(x@theses[,i])
              return(exclusion)
            }
            exclusion <- aaply(1:ncol(x@theses), .margins=1, .fun=exclusion.fun)
            coef.plot <- function(i){
              plot(density(rnorm(n=nrow(x@x), mean=x@exp.vals[i], sd=x@conditional.sds[i])), 
                   main=paste("Variable", i), xlab="", ylab="")
              segments(0,0,0,exclusion[i], lwd=3)
            }
            l_ply(1:length(x@exp.vals), coef.plot)
          })

