#' A bma class
#' 
#' Object of class \code{bma} as created by the \code{fitBMA} functions
#'
#' 
#' An object of the class `bma' has the following slots:
#' \itemize{
#' \item \code{x} A matrix of covariates
#' \item \code{y} A vector of the dependent variable
#' \item \code{coefs} Coefficients in all models
#' \item \code{standardErrors} Standard errors of coefficients in all models
#' \item \code{r2s} R squared values for all models
#' \item \code{postProb} Posterior probability of each model
#' \item \code{postProbcoefs} Posterior probability that the coefficient is included
#' \item \code{bfVec} Bayes factor of each model
#' \item \code{expB} Expected values of coefficients
#' \item \code{expBcond} Expected values of coefficients conditional on the variable being included
#' \item \code{largerZero} Conditional posterior probability that the coefficient is larger than zero
#' \item \code{condSE} Standard error of coefficients conditional on the variable being included
#' \item \code{coefMatrix} Matrix of coefficients
#' \item \code{sdMatrix} Matrix of standard errors
#' }
#'
#' @author Jacob Montgomery, Dino Hadzic, Jae Hee Jung, Emily Moore
#' @export
setClass(Class="bma",
         representation = representation(
           x="matrix", 
           y="numeric", 
           coefs="list",
           standardErrors="list",
           r2s="numeric",
           postProbcoefs="matrix",
           bfVec="numeric",
           expB="matrix",
           expBcond="numeric",
           postProb="matrix",
           largerZero="numeric",
           condSE="numeric",
           coefMatrix="matrix",
           sdMatrix="matrix"
         ),
         prototype = prototype(
             x=matrix(), 
             y=numeric(), 
             coefs=list(),
             standardErrors=list(),
             r2s=numeric(),
             postProb=matrix(),
             postProbcoefs=matrix(),
             bfVec=numeric(),
             expB=matrix(),
             expBcond=numeric(),
             largerZero=numeric(),
             condSE=numeric(),
             coefMatrix=matrix(),
             sdMatrix=matrix()
         )
)

#' @export
setMethod("initialize", "bma",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

##commented out show for now
#' #@export
#setMethod("show", "bma",
          #function(object){
           # cat("Regression Fits:", "\n")
            #print(object@combo.fit)
            #cat("\n")
            #cat("Posterior Model Odds:", "\n")
            #print(object@bmk)
            #cat("\n")
            #cat("Posterior Expected Value of Coefficients:", "\n")
            #print(object@exp.vals)
            #cat("\n")
            #cat("Posterior Probability Coefficient is Non-zero:", "\n")
            #print(object@coefprobs)
          #}
#)





