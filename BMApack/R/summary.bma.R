#' summary.bma Function
#'
#' Summarizes bma objects as returned by fitBMA.
#'
#'
#' @return Prints summaries of coefficients and posterior model odds and returns list 
#'  \item{Conditional Mean}{Expected values of coefficients}
#'  \item{Conditional SD}{Standard error of coefficients conditional on the variable being included}
#'  \item{p(beta!=0|Y)}{Posterior probability that the coefficient is included}
#'  \item{p(beta>0|Y,M)}{Conditional posterior probability that the coefficient is larger than zero}
#' @author Jacob Montgomery, Jae Hee Jung, Emily Moore, Dino Hadzic
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
#' summary(BMAObject)
#' @rdname summary.bma
#' @aliases summary.bma,ANY-method summary.bma
#' @export
#' 

setMethod(f="summary", signature="bma",
          definition=function(object){

##The summary function returns a matrix (table) with the posterior expected value 
##of each coefficient, the conditional standard error of each coefficient, the posterior probability that each coefficient is 
##non-zero, and the posterior probability that the coefficient is larger than zero.
            
return(matrix(c(object@expBcond,
                object@condSE,
                object@postProbcoefs,
                object@largerZero),
              ncol=4,
              dimnames=list(rownames(object@expB),
                            c("Conditional mean","Conditional SD","p(beta!=0|Y)","p(beta>0|Y,M)"))))
          })

