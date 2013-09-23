\name{xWhile}
\alias{xWhile}
\title{Iteratively apply a function while a predicate is met.}
\usage{
  xWhile(pred, fn, init)
}
\arguments{
  \item{pred}{a unary function that returns a logical
  value, or a symbol or name identifying such a function.}

  \item{fn}{an arbitrary function, or a symbol or name
  identifying such a function.}

  \item{x}{an arbitrary value.}
}
\value{
  the result of successively applying \code{fn} to
  \code{x}.
}
\description{
  Iteratively apply a function while a predicate is met.
}
\section{Corner Cases}{
  length-zero values of \code{x} are handled normally,
  since \code{x} is an arbitrary value. Potentially
  non-terminating.
}
\seealso{
  Other arrow-maps: \code{\link{xUntil}}
}

