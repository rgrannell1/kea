\name{xLocater}
\alias{xLocater}
\alias{xLocater...}
\title{xLocater}
\usage{
xLocater(pred, coll)

xLocater...(pred, ...)
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
Get the position of the last element for which a predicate
returns true.
}
\section{Corner Cases}{
  returns integer(0) if no match is found.
}
\examples{

NULL

}

