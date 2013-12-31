\name{xImplode}
\alias{xImplode}
\alias{xImplode...}
\title{xImplode}
\usage{
xImplode(str, strs)

xImplode...(str, ...)
}
\arguments{
  \item{str}{a string to use as a delimiter.}

  \item{strs}{several character vectors.}

  \item{...}{see above.}
}
\value{
a length-one character vector.
}
\description{
Concatenate a character vector into a single string using a
delimiter.
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

  \code{ xMap...(\cr xGetKey('a'),\cr list(a = 1, b =
  1),\cr list(a = 2, b = 1))}

  instead of

  \code{ xMap(\cr xGetKey('a'),\cr list(\cr list(a = 1, b =
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
\code{\link{xExplode}}; \code{\link{xLines}};
\code{\link{xSubString}}, \code{\link{xSubString...}};
\code{\link{xUnchars}}, \code{\link{xUnchars...}};
\code{\link{xUnlines}}, \code{\link{xUnlines...}};
\code{\link{xUnwords}}, \code{\link{xUnwords...}};
\code{\link{xWords}}
}

