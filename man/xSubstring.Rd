\name{xSubstring}
\alias{xSubstring}
\alias{xSubstring...}
\title{xSubstring}
\usage{
xSubstring(str, nums)

xSubstring...(str, ...)
}
\arguments{
  \item{str}{a string.}

  \item{nums}{indices of \code{str}.}

  \item{...}{see above.}
}
\value{
A character vector.
}
\description{
Subset a string using normal R vector indexing.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other character_functions: \code{\link{xExplode}},
\code{\link{xExplode...}}; \code{\link{xFromChars}},
\code{\link{xFromChars...}}; \code{\link{xFromLines}},
\code{\link{xFromLines...}}; \code{\link{xFromWords}},
\code{\link{xFromWords...}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xToChars}};
\code{\link{xToLines}}; \code{\link{xToWords}}
}

