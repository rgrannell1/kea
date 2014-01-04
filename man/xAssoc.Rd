\name{xAssoc}
\alias{xAssoc}
\alias{xAssoc...}
\title{xAssoc}
\usage{
xAssoc(colls)

xAssoc...(...)
}
\arguments{
  \item{colls}{a list or pairlist of list or pairlist
  pairs, with the first element being a string and the
  second element being any value.}

  \item{...}{see above.}
}
\value{
a named list.
}
\description{
Convert a list of name, value pairs into a named list.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\examples{

}
\seealso{
Other name_functions: \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xGetKey}};
\code{\link{xName}}; \code{\link{xPluck}},
\code{\link{xPluck...}}

Other reshaping_functions: \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xSegment}}, \code{\link{xSegment...}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xSplit}}, \code{\link{xSplit...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

