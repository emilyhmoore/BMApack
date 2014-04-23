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
and returns list \item{Conditional Mean}{Expected values of
coefficients} \item{Conditional SD}{Standard error of
coefficients conditional on the variable being included}
\item{p(beta!=0|Y)}{Posterior probability that the
coefficient is included} \item{p(beta>0|Y,M)}{Conditional
posterior probability that the coefficient is larger than
zero}
}
\description{
Summarizes bma objects as returned by fitBMA.
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
summary(BMAObject)
}
\author{
Jacob Montgomery, Jae Hee Jung, Emily Moore, Dino Hadzic
}

