\name{xForall}
\alias{xForall}
\alias{xForall...}
\title{xForall}
\usage{
xForall(pred, colls)

xForall...(pred, ...)
}
\arguments{
  \item{pred}{an predicate function that takes as many
  arguments as there are collections in \bold{colls}.}

  \item{colls}{a collection of collections.}

  \item{...}{see above.}
}
\value{
A boolean value.
}
\description{
Test if every ordered group of arguments from several
collections is true for a predicate.
}
\details{
\bold{xForall} can be used to test if every element of a
collection is true (like the base function \bold{all( )})
by simply using it with the identity function.

\code{coll <- c(True, False, True, True)}

\code{xForall...(xIdentity, coll)}

When supplied with a collection of multiple collections the
set product of those collections is checked with the
predicate. In order for \bold{xForall} to return true every
element of the set product must return true for a
predicate.

\code{commutes <- (a : b) := { a + b == b + a }}

\code{xForall...(commutes, 1:2, 1:2)}

The set product is

\code{list(list(1, 1), list(1, 2), list(2, 1), list(2, 2))}

Each member of the set product is tested with the
\bold{commutes} function

\code{list(commutes(1, 1), commutes(1, 2), commutes(2, 1),
commutes(2, 2))}

\code{list(True, True, True, True)}

\code{True}
}
\section{Corner Cases}{
  If any collection is length zero (or no collections are
  given), then logical(0) is returned.
}
\examples{

# 1. Detect if a sentence without punctuation is palindromic.

palindromic <- "Marge let a moody baby doom a telegram."

left_to_right <-
x_(palindromic) $
xToChars() $
xMap(tolower) $
x_Select(
	function (char) xIsMember(char, letters)
)

right_to_left <- x_(left_to_right) $ x_Reverse()

xForall...(
	ith := {
		xAsCharacter(left_to_right[ith]) ==
		xAsCharacter(right_to_left[ith])
	},
	seq_along(left_to_right)
)
}
\seealso{
Other quantifier_functions: \code{\link{xExists}},
\code{\link{xExists...}}
}

