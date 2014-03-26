#' Print.regcombo Function
#'
#' Prints regcombo objects as returned by fitBMA
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
#' @rdname print.regcombo
#' @export
#' 

setMethod(f="print", signature="regcombo",
          definition=function(x, ...){
            cat("Regression Fits:", "\n")
            print.default(object@combo.fit)
            cat("\n")
            cat("Posterior Model Odds:", "\n")
            print.default(object@bmk)
            cat("\n")
            cat("Posterior Expected Value of Coefficients:", "\n")
            print.default(object@exp.vals)
            cat("\n")
            cat("Posterior Probability Coefficient is Non-zero:", "\n")
            print.default(object@coefprobs)
          })
