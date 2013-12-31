\name{xZipWith}
\alias{xZipWith}
\alias{xZipWith...}
\title{xZipWith}
\usage{
xZipWith(fn, colls)

xZipWith...(fn, ...)
}
\arguments{
  \item{fn}{an n-ary function, or a symbol or name
  identifying such a function.}

  \item{colls}{n-vectors, lists or pairlists.}

  \item{...}{see above.}
}
\value{
returns a list of equal length to the shortest input
collection.
}
\description{
Generate a list of n-element lists from n collections, and
apply a function to each n-element list.
}
\section{Corner Cases}{
  the empty list is returned if the shortest collection has
  length-zero, or no collections are included. Each
  collection is truncated to the length of the shortest
  collection.
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
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xSegment}}, \code{\link{xSegment...}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xSplit}}, \code{\link{xSplit...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

