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
a character vector.
}
\description{
Subset a string using normal R vector indexing.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other character_functions: \code{\link{xChars}};
\code{\link{xExplode}}, \code{\link{xExplode...}};
\code{\link{xImplode}}, \code{\link{xImplode...}};
\code{\link{xLines}}; \code{\link{xUnchars}},
\code{\link{xUnchars...}}; \code{\link{xUnlines}},
\code{\link{xUnlines...}}; \code{\link{xUnwords}},
\code{\link{xUnwords...}}; \code{\link{xWords}}
}

