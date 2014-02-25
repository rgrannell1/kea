\name{xZip}
\alias{xZip}
\alias{xZip...}
\title{xZip}
\usage{
xZip(colls)

xZip...(...)
}
\arguments{
  \item{colls}{a collection of collections of equal
  lengths.  The collections to zip together.}

  \item{...}{see above.}
}
\value{
Returns a list of lists.
}
\description{
Generate a list of n-element lists from n collections.
}
\details{
\bold{xZip} converts the 'columns' of a collection of
collections (each inner collection) to 'rows.'

\code{coll <- list( list(1, 2, 3), list('a', 'b', 'c') )}

\code{xZip(coll)}

\code{list( list(1, 'a'), list(2, 'b'), list('c') )}

In the above case the 'columns' - a list of numbers and a
list of letters - were zipped into corresponding 'rows' of
a number and a letter.

\bold{xZip} is an involution - a function that is its own
inverse. For any collection, \bold{xZip(xZip(coll))} is the
original collection.

Applying \bold{xZip} again will reconvert the 'rows' back
into columns.

\code{coll <- list( list(1, 2, 3), list('a', 'b', 'c') )}

\code{xZip(coll)}
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
Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xOneOf}},
\code{\link{xOneOf...}}; \code{\link{xRecycle}},
\code{\link{xRecycle...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}},
\code{\link{xReverse...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSortBy}},
\code{\link{xSortBy...}}; \code{\link{xSplitAt}},
\code{\link{xSplitAt...}}; \code{\link{xSplitBy}},
\code{\link{xSplitBy...}}; \code{\link{xTabulate}},
\code{\link{xTabulate...}}; \code{\link{xUnzipNames}},
\code{\link{xUnzipNames...}}; \code{\link{xZipNames}},
\code{\link{xZipNames...}}
}

