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
\examples{


# 1. HR Functional Programming
#    Count the number of elements in a list without using length

# This problem is simple, but involves a slight misuse of xPoll. xPoll checks
# each element of a collection with a function and counts the number of times that
# predicate returns true. If we force every element polled to return true we
# can count the number of elements in a collection.

# xK(TRUE) returns a function that always returns TRUE. If we partially apply
# xPoll with this predicate we have a new length function.

lenof <- xPartial...(xPoll, pred = xK(TRUE))

lenof(c(1,2,3,4))
# 4
}

