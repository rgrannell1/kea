\name{xTabulate}
\alias{xTabulate}
\alias{xTabulate...}
\title{xTabulate}
\usage{
xTabulate(coll)

xTabulate...(...)
}
\arguments{
  \item{coll}{a collection. The values to find the
  frequency of.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Tabulate a collection into pairs of value:frequency lists.
}
\details{
\bold{xTabulate} is superficially similar to the base
function \bold{table}: given a collection that may or may
not contain duplicates, it calculates the frequencies of
each unique element.

The result of the tabulation is unsorted.
}
\section{Corner Cases}{
  If \bold{coll} is length-zero the empty list is returned.
}
\examples{

NULL
}

