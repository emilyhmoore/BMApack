\name{plot}
\alias{plot}
\title{plot.bma Function}
\description{
  Plots bma objects as returned by fitBMA. Plots the
  density of the normal distribution centered at expected
  coefficient and with a standard deviation equal to the
  weighted standard error of variable. Also plots line
  segment centered at zero indicating probability that
  coefficient is zero. Will need to scroll through the
  plots, each corresponding to a particular variable.
}
\examples{
data<-matrix(rnorm(1000), ncol=10)
colnames(data)<-c(paste("x", 1:10, sep=""))
datay<-5*data[,2]+3*data[,3]+rnorm(100)
trial<-(fitBMA(x=data, y=datay, g=3, parallel=FALSE))
plot(trial)
}
\author{
  Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily
  Moore
}

