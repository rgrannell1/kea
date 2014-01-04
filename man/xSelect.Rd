\name{xSelect}
\alias{xFilter}
\alias{xFilter...}
\alias{xSelect}
\alias{xSelect...}
\title{xSelect}
\usage{
xSelect(pred, coll)

xFilter(pred, coll)

xSelect...(pred, ...)

xFilter...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Include all elements from a collection matching a
predicate.
}
\section{Uses}{
  The select function is useful for taking a collection,
  and returning values that meet certain criteria. Likely
  uses include selecting rows in a (converted) data frame
  that contain a certain value, selecting strings that
  match a regular expression, or selecting records based on
  a value in a particular field.
}

\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero, or
  no match is found.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other filtering_functions: \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartition}};
\code{\link{xReject}}, \code{\link{xReject...}}
}

