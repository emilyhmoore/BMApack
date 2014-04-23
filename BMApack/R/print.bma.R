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
#' x <- matrix(rnorm(220), ncol=11)
#' colnames(x) <- paste("X", 1:11,sep="")
#' y <- rnorm(20)
#' allNothing <- list(c("X1","X2"))
#' eitherOr <- list(c("X3", "X4"))
#' always <- "X5"
#' conditionals <- list(c("X6", "X7"))
#' conditionedOnTheseVariables <- list(c("X8","X9"))
#' BMAObject <- fitBMA(x, y, g=3, parallel=FALSE, allNothing, eitherOr, always,conditionals,conditionedOnTheseVariables)
#' print(BMAObject)
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
