#' Print.bma Function
#'
#' Prints bma objects as returned by fitBMA.
#'
#'
#' @return Prints summaries of coefficients and posterior model odds and returns list 
#'  \item{expB}{A vector of expected coefficient values}
#'  \item{postProbcoefs}{A vector of probabilities that the coefficient is non-zero}
#' @author Jacob Montgomery, Emily Moore, Jae Hee Jung, Dino Hadzic
#' @examples
#' 
#' x1<-rnorm(500)
#' x2<-rnorm(500,3,15)
#' dep<-(x1+2*x2)+rnorm(500,4,100)
#' covars<-cbind(x1,x2) 
#' trial<-fitBMA(x=covars, y=dep)
#' print(trial)
#' @rdname print.bma
#' @aliases print.bma,ANY-method print.bma
#' @export
#' 

setMethod(f="print", signature="bma",
          definition=function(x, ...){
            cat("Posterior Expected Value of Coefficients:", "\n")
            print.default(x@expB)
            cat("\n")
            cat("Summary Stats for Posterior Probability Coefficient is Non-zero:", "\n")
            print.default(x@postProbcoefs)
          })
