\name{xNotFalse}
\alias{xNotFalse}
\alias{xNotFalse...}
\title{xNotFalse}
\usage{
xNotFalse(coll)

xNotFalse...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A vector of boolean values.
}
\description{
Is an element of a collection not false?
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero.
}

