\name{xToLines}
\alias{xToLines}
\title{xToLines}
\usage{
xToLines(str)
}
\arguments{
  \item{str}{a length-one character vector. The string to
  split into lines.}
}
\value{
A character vector.
}
\description{
Split a string at every newline character.
}
\details{
The input string is split at newline characters.  Multiple
newlines are treated the same as one newline.
}
\examples{

# 1. Lexically sort a list.
#

info <-
"
a: a for aardvark.
z: z for zebra.
d: d for deer.
f: f for ferret.
g: g for green snake.
y: y for yapok.
"

x_(info) $ xToLines() $ x_Tap(sort)
}
\seealso{
Other text_processing_functions: \code{\link{xExplode}},
\code{\link{xExplode...}}; \code{\link{xFromChars}},
\code{\link{xFromChars...}}; \code{\link{xFromLines}},
\code{\link{xFromLines...}}; \code{\link{xFromWords}},
\code{\link{xFromWords...}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xReadChars}},
\code{\link{xReadLines}}; \code{\link{xReadWords}};
\code{\link{xSubstring}}, \code{\link{xSubstring...}};
\code{\link{xToChars}}; \code{\link{xToWords}};
\code{\link{xWriteChars}}; \code{\link{xWriteLines}};
\code{\link{xWriteWords}}
}

