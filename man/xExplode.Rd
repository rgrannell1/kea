\name{xExplode}
\alias{xExplode}
\alias{xExplode...}
\title{xExplode}
\usage{
xExplode(rexp, str)

xExplode...(rexp, ...)
}
\arguments{
  \item{rexp}{a regular expression.}

  \item{str}{a string.}
}
\value{
a character vector.
}
\description{
Split a string into a character vector using a regular
expression.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other character_functions: \code{\link{xFromChars}},
\code{\link{xFromChars...}}; \code{\link{xFromLines}},
\code{\link{xFromLines...}}; \code{\link{xFromWords}},
\code{\link{xFromWords...}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xSubstring}},
\code{\link{xSubstring...}}; \code{\link{xToChars}};
\code{\link{xToLines}}; \code{\link{xToWords}}
}

