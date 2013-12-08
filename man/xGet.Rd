\name{xGet}
\alias{xGet}
\title{xGet}
\usage{
  xGet(str)
}
\arguments{
  \item{str}{a string.}
}
\value{
  a unary function that takes a collection.
}
\description{
  Return a function that selects a key from a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

