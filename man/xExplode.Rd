\name{xExplode}
\alias{xExplode}
\alias{xExplode...}
\title{xExplode}
\usage{
xExplode(rexp, str)

xExplode...(rexp, ...)
}
\arguments{
  \item{rexp}{a regular expression. The pattern at which to
  split \bold{str}.}

  \item{str}{a string. The string to split.}

  \item{...}{see above}
}
\value{
A character vector.
}
\description{
Split a string into a character vector using a regular
expression.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.  If
  no match is found the original string is returned.  Every
  element of the returned character vector has one or more
  characters - no zero-character elements are ever
  generated.
}
\examples{

NULL

}
\seealso{
Other text_processing_functions: \code{\link{xFromChars}},
\code{\link{xFromChars...}}; \code{\link{xFromLines}},
\code{\link{xFromLines...}}; \code{\link{xFromWords}},
\code{\link{xFromWords...}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xReadChars}};
\code{\link{xReadLines}}; \code{\link{xReadWords}};
\code{\link{xSubstring}}, \code{\link{xSubstring...}};
\code{\link{xToChars}}; \code{\link{xToLines}};
\code{\link{xToWords}}; \code{\link{xWriteChars}};
\code{\link{xWriteLines}}; \code{\link{xWriteWords}}
}

