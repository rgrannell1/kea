\name{xZip}
\alias{xZip}
\alias{xZip...}
\title{xZip}
\usage{
xZip(colls)

xZip...(...)
}
\arguments{
  \item{colls}{n-vectors, lists or pairlists.}

  \item{...}{see above.}
}
\value{
Returns a list.
}
\description{
Generate a list of n-element lists from n collections.
}
\section{Corner Cases}{
  The empty list is returned if the shortest collection has
  length-zero, or no collections are included.
}
\examples{

# seperate a csv of country,median age pairs
# into a list of countries and ages.
# this is a nicer format for Arrow to work with
# than a string or data.frame.

csv <- "Monaco,48.9,Japan,44.6,Italy,44.3,Germany,43.7,Jersey,43.4"

x_(csv) $ xExplode(',') $ xChunk(2) $ xZip() $
x_AsNamed(c('country', 'age'))

# list(
#     country =
#     list(
#         "Monaco",
#         "Japan",
#         "Italy",
#         "Germany",
#         "Jersey"),
#     age =
#     list(
#         "48.9",
#         "44.6",
#         "44.3",
#         "43.7",
#         "43.4"
# ))

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}}
}

