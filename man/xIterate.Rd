\name{xIterate}
\alias{xIterate}
\title{xIterate}
\usage{
xIterate(fn, init)
}
\arguments{
  \item{fn}{a function.}

  \item{init}{an arbitrary value.}
}
\value{
the result of successively applying \code{f} to
\code{init}.
}
\description{
Iteratively apply a function to a value.
}
\section{Corner Cases}{
  length-zero values of \code{init} are handled normally,
  since \code{init} is an arbitrary value. Potentially
  non-terminating.
}

