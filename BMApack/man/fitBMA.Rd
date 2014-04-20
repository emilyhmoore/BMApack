\name{fitBMA}
\alias{fitBMA}
\title{fitBMA Function}
\usage{
fitBMA(x, y, g = 3, parallel = TRUE, allNothing = NULL, eitherOr = NULL,
  always = NULL)
}
\arguments{
  \item{x:}{A numeric matrix of covariates}

  \item{y:}{A numeric vector of the dependent variable}

  \item{g:}{A value for the hyper-prior g}

  \item{parallel:}{runs in parallel if TRUE}

  \item{allNothing:}{a vector of the names of covariates
  that should all be included or none should be included}

  \item{eitherOr:}{a vector of the names of covariates
  among which only one should be included or none should be
  included}

  \item{always:}{a vector of the names of covariates that
  should always be included}
}
\value{
An bma class object with the following slots: \item{x}{A
matrix of covariates} \item{y}{A vector of the dependent
variable} \item{coefs}{Coefficients in all models}
\item{standardErrors}{Standard errors of coefficients in
all models} \item{r2s}{R squared values for all models}
\item{postProb}{Posterior probability of each model}
\item{postProbcoefs}{Posterior probability that the
coefficient is included} \item{bfVec}{Bayes factor of each
model} \item{expB}{Expected values of coefficients}
\item{expBcond}{Expected values of coefficients conditional
on the variable being included}
\item{largerZero}{Conditional posterior probability that
the coefficient is larger than zero} \item{condSE}{Standard
error of coefficients conditional on the variable being
included} \item{coefMatrix}{Matrix of coefficients}
\item{sdMatrix}{Matrix of standard errors}
}
\description{
Runs regression on all possible combinations of covariates
given the restrictions specified by the user
}
\examples{
x1<-rnorm(500)
x2<-rnorm(500,3,15)
dep<-(x1+2*x2)+rnorm(500,4,100)
covars<-cbind(x1,x2)
fitBMA(x=covars, y=dep, parallel=FALSE)
}
\author{
Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily
Moore
}

