\docType{methods}
\name{summary,bma-method}
\alias{summary,bma-method}
\alias{summary.bma}
\alias{summary.bma,ANY-method}
\title{summary.bma Function}
\usage{
\S4method{summary}{bma}(object)
}
\arguments{
  \item{object}{an object for which a summary is desired.}
}
\value{
Prints summaries of coefficients and posterior model odds
and returns list \item{expB}{Expected values of
coefficients} \item{condSE}{Standard error of coefficients
conditional on the variable being included}
\item{postProbcoefs}{Posterior probability that the
coefficient is included} \item{largerZero}{Conditional
posterior probability that the coefficient is larger than
zero}
}
\description{
Summarizes bma objects as returned by fitBMA
}
\examples{
x1<-rnorm(500)
x2<-rnorm(500,3,15)
dep<-(x1+2*x2)+rnorm(500,4,100)
covars<-cbind(x1,x2)
trial<-fitBMA(x=covars, y=dep)
summary(trial)
}
\author{
Jacob Montgomery, Jae Hee Jung, Emily Moore, Dino Hadzic
}

