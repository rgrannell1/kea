\name{xRepeat}
\alias{xRepeat}
\alias{xRepeat...}
\title{xRepeat}
\usage{
xRepeat(num, coll)

xRepeat...(num, ...)
}
\arguments{
  \item{num}{a nonnegative positive number}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Repeat a collection several times.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero or
  num is zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\examples{

# Example 1
#
# Use xRepeat to repeat a sine wave.

range <- seq(0, 2*pi, len = 600)
wave <- xRepeat(5, sin(range))

plot(unlist(wave), type = 'l')

# Example 2
#
# Use xRepeat to build up a nested list.

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xGroup}},
\code{\link{xGroup...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

