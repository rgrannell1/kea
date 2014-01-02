\name{xUnwords}
\alias{xUnwords}
\alias{xUnwords...}
\title{xUnwords}
\usage{
xUnwords(strs)

xUnwords...(...)
}
\arguments{
  \item{strs}{an arbitrary number of character vectors.}

  \item{...}{see above.}
}
\value{
a length-one character vector.
}
\description{
Collapase a character vector with spaces as delimiters.
}
\section{Appendix-Variadic functions}{
  Many arrow functions come in two forms:

  \itemize{ \item{\bold{xMethod:}}{ These functions do not
  collect arguments with the '...' parametre.}
  \item{\bold{xMethod...:}}{ These functions collect
  arguments with the '...' parametre.} }

  The xMethod... form is somewhat shorter to write, but it
  is mainly used when the arrow function will take a fixed
  number of arguments. An ideal use-case would be writing

  \code{ xMap...( xGetKey('a'), list(a = 1, b = 1), list(a
  = 2, b = 1))}

  instead of

  \code{ xMap( xGetKey('a'), list( list(a = 1, b = 1),
  list(a = 2, b = 1)) )}

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
\code{\link{xSubString}}, \code{\link{xSubString...}};
\code{\link{xUnchars}}, \code{\link{xUnchars...}};
\code{\link{xUnlines}}, \code{\link{xUnlines...}};
\code{\link{xWords}}
}

