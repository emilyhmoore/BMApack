\name{fitBMA}
\alias{fitBMA}
\title{fitBMA Function}
\usage{
fitBMA(x, y, g = 3, parallel = TRUE, allNothing = NULL, eitherOr = NULL,
  always = NULL, squaredInteraction = NULL, constituentTerms = NULL)
}
\arguments{
  \item{x:}{A numeric matrix of covariates}

  \item{y:}{A numeric vector of the dependent variable}

  \item{g:}{A value for the hyper-prior g}

  \item{parallel:}{Runs in parallel if TRUE}

  \item{allNothing:}{A list of the names of covariates that
  should all be included or none should be included}

  \item{eitherOr:}{A list of the names of covariates among
  which only one should be included or none should be
  included}

  \item{always:}{A vector of the names of covariates that
  should always be included}

  \item{squaredInteraction:}{A list of the names of
  covariates conditioned on the covariates included in
  constituentTerms. These may include squared terms and/or
  interaction terms.}

  \item{constituentTerms:}{A list of the names of
  covariates that serve as constitutive terms for
  covariates included in squaredInteraction.}
}
\value{
A bma class object with the following slots: \item{x}{A
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
x <- matrix(rnorm(220), ncol=11)
colnames(x) <- paste("X", 1:11,sep="")
y <- rnorm(20)
allNothing <- list(c("X1","X2"))
eitherOr <- list(c("X3", "X4"))
always <- "X5"
squaredInteraction <- list(c("X6", "X7"))
constituentTerms <- list(c("X8","X9"))
fitBMA(x, y, g=3, parallel=FALSE, allNothing, eitherOr, always,squaredInteraction,constituentTerms)
}
\author{
Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily
Moore
}

