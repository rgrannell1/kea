\name{xLocate}
\alias{xLocate}
\alias{xLocate...}
\alias{xLocatel}
\alias{xLocatel...}
\title{xLocate}
\usage{
xLocate(pred, coll)

xLocatel(pred, coll)

xLocatel...(pred, ...)

xLocate...(pred, ...)
}
\arguments{
  \item{pred}{a predicate function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
An length-one or length-zero whole number.
}
\description{
Get the position of the first element for which a predicate
returns true.
}
\section{Corner Cases}{
  returns integer(0) if no match is found.
}

