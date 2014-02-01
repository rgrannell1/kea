\name{xUnit}
\alias{xUnit}
\title{xUnit}
\usage{
xUnit(coll)
}
\arguments{
  \item{coll}{an list, pairlist, or vector of any length.
  The collection to return the length-zero unit element
  of.}
}
\value{
Returns null if \bold{coll} is a pairlist, a typed vector
of length zero if \bold{coll} is a vector, and the empty
list if \bold{coll} is a list.
}
\description{
Return the empty version of a collection.
}
\examples{

# 1. HR Functional Programming
#    Reverse a list without using reverse

# Arrow has a function for reversing a list, but it is trivial to reverse a list with
# xFold. The function xFold needs to be given a value to return if the input collection
# is length-zero; xUnit returns the empty version of a collection.

# To do a very tidy job we use xUnit to return the correct length-zero value
# depending on the type of collection given. If an integer vector is given,
# xUnit will return integer(0), and so on for other vectors.

reverse <- coll := {
	xFoldl(
		(left : right) := {
			c(right, left)
		},
		xUnit(coll),
		coll
	)
}

reverse(1:4)
# c(4, 3, 2, 1)
}

