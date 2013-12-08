\name{xVectorise}
\alias{xVectorise}
\title{xVectorise}
\usage{
  xVectorise(fn)
}
\arguments{
  \item{fn}{a unary function.}
}
\value{
  a unary function of coll.
}
\description{
  Partially apply xMap with a function.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

