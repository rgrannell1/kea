% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/xIndicesOf.R
\name{xIndicesOf}
\alias{xIndicesOf}
\alias{xIndicesOf_}
\title{xIndicesOf}
\usage{
xIndicesOf(coll)

xIndicesOf_(...)
}
\arguments{
\item{coll}{a collection. The collection to get the indices of.}

\item{...}{see above.}
}
\value{
an integer vector.
}
\description{
Get the indices of a collection.
}
\section{Type Signature}{

    |any| -> <integer>
}

\section{Corner Cases}{

    Returns \bold{integer(0)} when \bold{coll} is length-zero.
}
\examples{

# 1. get the indices of a collection.

xIndicesOf(letters)

# 1:26
}
\seealso{
Other key_functions: \code{\link{xAddKeys}},
  \code{\link{xAddKeys_}}; \code{\link{xIndicesTo}};
  \code{\link{xKeysOf}}, \code{\link{xKeysOf_}};
  \code{\link{xLocate}}, \code{\link{xLocate_}};
  \code{\link{xUnzipIndices}},
  \code{\link{xUnzipIndices_}}; \code{\link{xUnzipKeys}},
  \code{\link{xUnzipKeys_}}; \code{\link{xValuesOf}},
  \code{\link{xValuesOf_}}; \code{\link{xZipKeys}},
  \code{\link{xZipKeys_}}
}

