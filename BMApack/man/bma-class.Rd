\docType{class}
\name{bma-class}
\alias{bma-class}
\title{A bma class}
\description{
Object of class \code{bma} as created by the \code{fitBMA}
functions
}
\details{
An object of the class `bma' has the following slots:
\itemize{ \item \code{x} A matrix of covariates \item
\code{y} A vector of the dependent variable \item
\code{coefs} Coefficients in all models \item
\code{standardErrors} Standard errors of coefficients in
all models \item \code{r2s} R squared values for all models
\item \code{postProb} Posterior probability of each model
\item \code{postProbcoefs} Posterior probability that the
coefficient is included \item \code{bfVec} Bayes factor of
each model \item \code{expB} Expected values of
coefficients \item \code{expBcond} Expected values of
coefficients conditional on the variable being included
\item \code{largerZero} Conditional posterior probability
that the coefficient is larger than zero \item
\code{condSE} Standard error of coefficients conditional on
the variable being included \item \code{coefMatrix} Matrix
of coefficients \item \code{sdMatrix} Matrix of standard
errors }
}
\author{
Jacob Montgomery, Dino Hadzic, Jae Hee Jung, Emily Moore
}

