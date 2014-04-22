#' summary.bma Function
#'
#' Summarizes bma objects as returned by fitBMA
#'
#'
#' @return Prints summaries of coefficients and posterior model odds and returns list 
#'  \item{expB}{Expected values of coefficients}
#'  \item{condSE}{Standard error of coefficients conditional on the variable being included}
#'  \item{postProbcoefs}{Posterior probability that the coefficient is included}
#'  \item{largerZero}{Conditional posterior probability that the coefficient is larger than zero}
#' @author Jacob Montgomery, Jae Hee Jung, Emily Moore, Dino Hadzic
#' @examples
#' 
#' x1<-rnorm(500)
#' x2<-rnorm(500,3,15)
#' dep<-(x1+2*x2)+rnorm(500,4,100)
#' covars<-cbind(x1,x2) 
#' trial<-fitBMA(x=covars, y=dep)
#' summary(trial)
#' @rdname summary.bma
#' @export
#' 

setMethod(f="summary", signature="bma",
          definition=function(object){

##The summary function returns a matrix (table) with the posterior expected value 
##of each coefficient and the posterior probability that each coefficient is 
##non-zero listed by columns.
            
return(matrix(c(object@expBcond,
                object@condSE,
                object@postProbcoefs,
                object@largerZero),
              ncol=4,
              dimnames=list(rownames(object@expB),
                            c("Conditional mean","Conditional SD","p(beta!=0|Y)","p(beta>0|Y,M)"))))
          })

