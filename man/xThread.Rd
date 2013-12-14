\name{xThread}
\alias{xThread}
\title{xThread}
\usage{
xThread(init, fns)
}
\arguments{
  \item{init}{an arbitrary value}

  \item{fns}{several unary functions.}
}
\value{
a list.
}
\description{
Iteratively apply a value to list of functions.
}
\section{Corner Cases}{
  q returns the empty list if \code{coll} is length-zero.
}

