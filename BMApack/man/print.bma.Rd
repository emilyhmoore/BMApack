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
and returns list \item{expB}{A vector of expected
coefficient values} \item{postProbcoefs}{A vector of
probabilities that the coefficient is non-zero}
}
\description{
Prints bma objects as returned by fitBMA.
}
\examples{
x <- matrix(rnorm(220), ncol=11)
colnames(x) <- paste("X", 1:11,sep="")
y <- rnorm(20)
allNothing <- list(c("X1","X2"))
eitherOr <- list(c("X3", "X4"))
always <- "X5"
conditionals <- list(c("X6", "X7"))
conditionedOnTheseVariables <- list(c("X8","X9"))
BMAObject <- fitBMA(x, y, g=3, parallel=FALSE, allNothing, eitherOr, always,conditionals,conditionedOnTheseVariables)
print(BMAObject)
}
\author{
Jacob Montgomery, Emily Moore, Jae Hee Jung, Dino Hadzic
}

