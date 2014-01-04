\name{xPermute}
\alias{xPermute}
\alias{xPermute...}
\title{xPermute}
\usage{
xPermute(coll, colls)

xPermute...(coll, ...)
}
\arguments{
  \item{coll}{a collection of whole numbers.}

  \item{colls}{several collections of equal length to
  \code{coll}.}

  \item{...}{see above.}
}
\value{
a list of collections.
}
\description{
Permute several collections using indices.
}
\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xSegment}}, \code{\link{xSegment...}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xSplit}}, \code{\link{xSplit...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

