#' A bma class
#' 
#' Object of class \code{bma} as created by the \code{fitBMA} functions
#'
#' 
#' An object of the class `bma' has the following slots:
#' \itemize{
#' \item \code{combo.coef} A list of coefficients for each possible regression
#' \item \code{combo.fit} A vector of R-Squared values
#' \item \code{bmk}
#' \item \code{exp.vals}
#' \item \code{thecoefs}
#' \item \code{coefprobs}
#' \item \code{coefprobs.largerthanzero}
#' \item \code{conditional.sds}
#' \item \code{x} A matrix input of covariates
#' \item \code{y} The dependent variable input (vector) 
#' }
#'
#' @author Jacob Montgomery, Dino Hadzic, Jae Hee Jung, Emily Moore
#' @export
setClass(Class="bma",
         representation = representation(
           combo.coef = "list",
           combo.fit="numeric",
           bmk="numeric",
           exp.vals="numeric",
           thecoefs="matrix",
           coefprobs="numeric",
           coefprobs.largerthanzero="numeric",
           conditional.sds="numeric",
           x="matrix",
           y="numeric"
         ),
         prototype = prototype(
           combo.coef=list(),
           combo.fit=numeric(),
           bmk=numeric(),
           exp.vals=numeric(),
           thecoefs=matrix(),
           coefprobs=numeric(),
           coefprobs.largerthanzero=numeric(),
           conditional.sds=numeric(),
           x = matrix(),
           y = numeric()
         )
)

#' @export
setMethod("initialize", "bma",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export
setMethod("show", "bma",
          function(object){
            cat("Regression Fits:", "\n")
            print(object@combo.fit)
            cat("\n")
            cat("Posterior Model Odds:", "\n")
            print(object@bmk)
            cat("\n")
            cat("Posterior Expected Value of Coefficients:", "\n")
            print(object@exp.vals)
            cat("\n")
            cat("Posterior Probability Coefficient is Non-zero:", "\n")
            print(object@coefprobs)
          }
)





