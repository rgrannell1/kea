\name{xPoll}
\alias{xPoll}
\alias{xPoll...}
\alias{xRemoveNa...}
\alias{xRemoveNan...}
\title{xPoll}
\usage{
xPoll(pred, coll)

xPoll...(pred, ...)

xRemoveNa...(...)

xRemoveNan...(...)
}
\arguments{
  \item{pred}{a unary predicate function. The function with
  which to poll each element of the input collection.}

  \item{coll}{a collection. The collection to poll.}

  \item{...}{see above.}
}
\value{
A non-negative whole number.
}
\description{
Count the number of times a function returns true when
mapped over a collection.
}

