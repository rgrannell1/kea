\name{xForall}
\alias{xForall}
\alias{xForall...}
\title{xForall}
\usage{
xForall(pred, colls)

xForall...(pred, ...)
}
\arguments{
  \item{pred}{an n-ary predicate.}

  \item{colls}{n collections.}

  \item{...}{see above.}
}
\value{
a boolean value.
}
\description{
Does ever selection of arguments satisfy a predicate?
}
\section{Corner Cases}{
  if any collection is length zero (or no collections are
  given), then False is returned.
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
Other quantifier_functions: \code{\link{xExists}},
\code{\link{xExists...}}
}

