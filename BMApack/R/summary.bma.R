#' summary.bma Function
#'
#' Summarizes bma objects as returned by fitBMA
#'
#'
#' @return Prints summaries of coefficients and posterior model odds and returns list 
#'  \item{exp.vals}{A vector of expected coefficient values}
#'  \item{coefprobs}{A vector of probabilities that the coefficient is non-zero}
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
            
return(matrix(c(object@exp.vals,object@conditional.sds,object@coefprobs,object@coefprobs.largerthanzero),ncol=4,
              dimnames=list(names(object@exp.vals),
                            c("Conditional mean","Conditional SD","p(beta!=0|Y)","p(beta>0|Y,M)"))))
          })

