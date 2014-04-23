\docType{methods}
\name{print,bma-method}
\alias{print,bma-method}
\alias{print.bma}
\alias{print.bma,ANY-method}
\title{Print.bma Function}
\usage{
\S4method{print}{bma}(x, ...)
}
\arguments{
  \item{x}{an object used to select a method.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
Prints summaries of coefficients and posterior model odds
and returns list \item{exp.vals}{A vector of expected
coefficient values} \item{coefprobs}{A vector of
probabilities that the coefficient is non-zero}
}
\description{
Prints bma objects as returned by fitBMA.
}
\examples{
x1<-rnorm(500)
x2<-rnorm(500,3,15)
dep<-(x1+2*x2)+rnorm(500,4,100)
covars<-cbind(x1,x2)
trial<-fitBMA(x=covars, y=dep)
print(trial)
}
\author{
Jacob Montgomery, Emily Moore, Jae Hee Jung, Dino Hadzic
}

