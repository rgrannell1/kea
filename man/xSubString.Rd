\name{xSubString}
\alias{xSubString}
\alias{xSubString...}
\title{xSubString}
\usage{
xSubString(str, nums)

xSubString...(str, ...)
}
\arguments{
  \item{str}{a string.}

  \item{nums}{indices of \code{str}.}
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

\section{Variadic functions}{
  Many arrow functions come in two forms:

  \itemize{ \item{\bold{xMethod:}}{ These functions do not
  collect arguments with the '...' parametre.}
  \item{\bold{xMethod...:}}{ These functions collect
  arguments with the '...' parametre} }

  The xMethod... form is somewhat shorter to write, but it
  is mainly used when the arrow function will take a fixed
  number of arguments. An ideal use-case would be writing

  \code{xMap...( xGetKey('a'),\cr list(a = 1, b = 1),\cr
  list(a = 2, b = 1))}

  instead of

  \code{xMap.( xGetKey('a'),\cr list(\cr list(a = 1, b =
  1),\cr list(a = 2, b = 1)) )}

  For other applications the xMethod form of the function
  is more likely to be useful:

  \code{nums <- (1:10) / 10}

  \code{xMap(sqrt, nums)}

  As opposed to

  \code{xApply(xMap..., list(sqrt, nums))}

  Arrow methods also have variadic and non-variadic forms
  in many cases.
}
\seealso{
Other character_functions: \code{\link{xChars}};
\code{\link{xExplode}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xLines}};
\code{\link{xUnchars}}, \code{\link{xUnchars...}};
\code{\link{xUnlines}}, \code{\link{xUnlines...}};
\code{\link{xUnwords}}, \code{\link{xUnwords...}};
\code{\link{xWords}}
}

