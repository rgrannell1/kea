\name{xSplitString}
\alias{xSplitString}
\title{xSplitString}
\usage{
xSplitString(rexp, str)
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
Other character_vector_functions: \code{\link{xChars}};
\code{\link{xCollapse}}; \code{\link{xLines}};
\code{\link{xSubString}}; \code{\link{xUnchars}};
\code{\link{xUnlines}}; \code{\link{xUnwords}};
\code{\link{xWords}}
}

