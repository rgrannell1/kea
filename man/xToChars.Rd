\name{xToChars}
\alias{xToChars}
\title{xToChars}
\usage{
xToChars(str)
}
\arguments{
  \item{str}{a length-one character vector. The string to
  split into characters.}
}
\value{
A character vector of length \code{nchar(str)}, with each
element being a single character.
}
\description{
Split a single string into a vector of characters.
}
\examples{

# 1. CE: Sum of Digits
# sum the digits of a positive integer

x_(toString(12358134L)) $
xToChars() $
xMap(as.numeric) $
x_Reduce("+")

# 27
}
\seealso{
Other character_functions: \code{\link{xExplode}},
\code{\link{xExplode...}}; \code{\link{xFromChars}},
\code{\link{xFromChars...}}; \code{\link{xFromLines}},
\code{\link{xFromLines...}}; \code{\link{xFromWords}},
\code{\link{xFromWords...}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xSubstring}},
\code{\link{xSubstring...}}; \code{\link{xToLines}};
\code{\link{xToWords}}
}

