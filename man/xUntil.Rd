\name{xUntil}
\alias{xUntil}
\title{Iteratively apply a function until a predicate is met.}
\usage{
  xUntil(pred, fn, init)
}
\arguments{
  \item{predicate}{a unary function that returns a logical
  value, or a symbol or name identifying such a function.}

  \item{unary}{a unary function function, or a symbol or
  name identifying such a function.}

  \item{init}{an arbitrary value.}
}
\value{
  the result of successively applying \code{f} to
  \code{init}.
}
\description{
  Iteratively apply a function until a predicate is met.
}
\section{Corner Cases}{
  length-zero values of \code{init} are handled normally,
  since \code{init} is an arbitrary value. Potentially
  non-terminating.
}
\seealso{
  Other arrow.maps: \code{\link{xWhile}}
}

