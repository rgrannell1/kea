\name{xIterate}
\alias{xIterate}
\title{xIterate}
\usage{
xIterate(fn, val)
}
\arguments{
  \item{fn}{a function.}

  \item{val}{an arbitrary value.}
}
\value{
the result of successively applying \code{f} to \code{val}.
}
\description{
Iteratively apply a function to a value.
}
\section{Corner Cases}{
  length-zero values of \code{val} are handled normally,
  since \code{val} is an arbitrary value. Potentially
  non-terminating.
}

