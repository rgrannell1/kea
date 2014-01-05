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
Other character_functions: \code{\link{xChars}};
\code{\link{xImplode}}, \code{\link{xImplode...}};
\code{\link{xLines}}; \code{\link{xSubstring}},
\code{\link{xSubstring...}}; \code{\link{xUnchars}},
\code{\link{xUnchars...}}; \code{\link{xUnlines}},
\code{\link{xUnlines...}}; \code{\link{xUnwords}},
\code{\link{xUnwords...}}; \code{\link{xWords}}
}

