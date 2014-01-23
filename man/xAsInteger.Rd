\name{xAsInteger}
\alias{xAsInteger}
\alias{xAsInteger...}
\title{xAsInteger}
\usage{
xAsInteger(nums)

xAsInteger...(...)
}
\arguments{
  \item{nums}{a collection of integer values. A list,
  pairlist or vector of length-one integer vectors to
  convert to a integer vector.}

  \item{...}{see above.}
}
\value{
An integer vector.
}
\description{
Convert a collection to a integer vector.
}
\details{
\code{xAsInteger} converts a list, pairlist or vector of
length-one integers to a integer vector. It does not
attempt to convert non-integer collections to integer
vectors.
}
\section{Corner Cases}{
  Double vectors are not coerced to integer vectors:
  doubles can be decimal or infinite numbers, which would
  be lost upon coersion.
}

