\docType{methods}
\name{plot,bma-method}
\alias{plot,bma-method}
\alias{plot.bma}
\alias{plot.bma,ANY-method}
\title{plot.bma}
\usage{
\S4method{plot}{bma}(x, y, ...)
}
\arguments{
  \item{x}{the coordinates of points in the plot.
  Alternatively, a single plotting structure, function or
  \emph{any \R object with a \code{plot} method} can be
  provided.}

  \item{y}{the y coordinates of points in the plot,
  \emph{optional} if \code{x} is an appropriate structure.}

  \item{...}{Arguments to be passed to methods, such as
  \link{graphical parameters} (see \code{\link{par}}).
  Many methods will accept the following arguments:
  \describe{ \item{\code{type}}{what type of plot should be
  drawn.  Possible types are \itemize{ \item \code{"p"} for
  \bold{p}oints, \item \code{"l"} for \bold{l}ines, \item
  \code{"b"} for \bold{b}oth, \item \code{"c"} for the
  lines part alone of \code{"b"}, \item \code{"o"} for both
  \sQuote{\bold{o}verplotted}, \item \code{"h"} for
  \sQuote{\bold{h}istogram} like (or \sQuote{high-density})
  vertical lines, \item \code{"s"} for stair \bold{s}teps,
  \item \code{"S"} for other \bold{s}teps, see
  \sQuote{Details} below, \item \code{"n"} for no plotting.
  } All other \code{type}s give a warning or an error;
  using, e.g., \code{type = "punkte"} being equivalent to
  \code{type = "p"} for S compatibility.  Note that some
  methods, e.g. \code{\link{plot.factor}}, do not accept
  this.  } \item{\code{main}}{an overall title for the
  plot: see \code{\link{title}}.} \item{\code{sub}}{a sub
  title for the plot: see \code{\link{title}}.}
  \item{\code{xlab}}{a title for the x axis: see
  \code{\link{title}}.} \item{\code{ylab}}{a title for the
  y axis: see \code{\link{title}}.} \item{\code{asp}}{the
  \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.} }
  }
}
\description{
Generates posterior coefficient plots which include the
posterior probability of the coefficient. Included on the
plot is a vertical line denoting the probability that the
variable is not in the model. User will need to scroll
through the various plots that are generated, each
corresponding to a particular variable.
}
\examples{
x <- matrix(rnorm(80),ncol=8)
colnames(x) <- paste("X",1:8,sep="")
y <- rnorm(10)
allNothing <- c("X1","X2")
eitherOr <- c("X5","X6")
always <- c("X3","X4")
BMAobject <- fitBMA(x=x, y=y, allNothing=allNothing, eitherOr=eitherOr, always=always)
plot(BMAobject)
}
\author{
Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily
Moore
}

