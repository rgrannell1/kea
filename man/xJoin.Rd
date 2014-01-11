\name{xJoin}
\alias{xJoin}
\alias{xJoin...}
\title{xJoin}
\usage{
xJoin(colls)

xJoin...(...)
}
\arguments{
  \item{colls}{several collections.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Concatenate several collections into one collection.
}
\section{Corner Cases}{
  Empty collections act as a unit for concatenation;
  concatenating the empty list to another list returns the
  second list, without modification.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xGroup}},
\code{\link{xGroup...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

