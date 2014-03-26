#' Print.bma Function
#'
#' Prints bma objects as returned by fitBMA
#'
#'
#' @return Prints summaries of coefficients and posterior model odds and returns list 
#'  \item{exp.vals}{A vector of expected coefficient values}
#'  \item{coefprobs}{A vector of probabilities that the coefficient is non-zero}
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
#' @export
#' 


setMethod(f="print", signature="bma",
          definition=function(x, ...){
            cat("Posterior Expected Value of Coefficients:", "\n")
            summary(object@exp.vals)
            cat("\n")
            cat("Posterior Probability Coefficient is Non-zero:", "\n")
            summary(object@coefprobs)
          })
