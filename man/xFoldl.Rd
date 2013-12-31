\name{xFoldl}
\alias{xFold}
\alias{xFoldl}
\alias{xFoldl...}
\title{xFoldl}
\usage{
xFoldl(fn, val, coll)

xFold(fn, val, coll)

xFoldl...(fn, val, ...)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its left argument.}

  \item{val}{an arbitrary value.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
an arbitrary value, depending on the function \code{fn}.
}
\description{
Successively combine a list of values into a single value
using a binary function (left to right, with an initial
value).
}
\section{Corner Cases}{
  returns \code{val} if \code{coll} is length-zero.
}

\section{Appendix-Variadic functions}{
  Many arrow functions come in two forms:

  \itemize{ \item{\bold{xMethod:}}{ These functions do not
  collect arguments with the '...' parametre.}
  \item{\bold{xMethod...:}}{ These functions collect
  arguments with the '...' parametre.} }

  The xMethod... form is somewhat shorter to write, but it
  is mainly used when the arrow function will take a fixed
  number of arguments. An ideal use-case would be writing

  \code{ xMap...(\cr xGetKey('a'),\cr list(a = 1, b =
  1),\cr list(a = 2, b = 1))}

  instead of

  \code{ xMap(\cr xGetKey('a'),\cr list(\cr list(a = 1, b =
  1),\cr list(a = 2, b = 1)) )}

  For other applications the xMethod form of the function
  is more likely to be useful:

  \code{nums <- (1:10) / 10}

  \code{xMap(sqrt, nums)}

  As opposed to

  \code{xApply(xMap..., list(sqrt, nums))}

  Arrow methods also have variadic and non-variadic forms
  in many cases.
}

\section{Appendix-Short-Circuiting}{
  The fold family of functions include an optional
  mechanism of prematurely returning an answer, similar to
  the \code{return()} function in base R. By calling
  \code{Return} with the value you want \code{fn} to yield
  instantly, the fold/reduce is halted, and that value is
  yielded as the result of that proceedure.

  \code{Return()} essentially acts as a higher-order return
  function, that breaks "further" than the normal return
  statement.

  \code{to_search <- c(list(0, 1, 0, 3, 2, 10), rep(4,
  1000))}

  \code{ xFoldl(\cr (checked : searched ) := {\cr if
  (searched == 10) Return(checked) else checked + 1\cr
  },\cr list(),\cr to_search\cr ) }

  \code{6}

  In this case, the Foldl returns its results prematurely
  after 6 searches, as opposed to the thousands of
  iterations that the base function \code{Reduce()} would
  have to preform before returning its result. By using
  \code{Return()} where possible folding functions can be
  made to preform significantly bettern
}
\examples{

# 1. sum a vector with fold.

xFold("+", 0, 1:10)

55

# 2. demonstrate how Return works by using
#    it to break out of the fold across 26 letters
#    prematurely.

xFold(
	(number_checked : letter) := {

		if (letter == 'q') {
			Return (number_checked)
		} else {
			number_checked + 1
		}
	},
	0,
	letters
)

# terminated after 16 checks, not 26.
16
}
\seealso{
Other folding_functions: \code{\link{xFoldList}},
\code{\link{xFoldList...}}, \code{\link{xFoldListl}},
\code{\link{xFoldListl...}}; \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xReducer}},
\code{\link{xReducer...}}; \code{\link{xReduce}},
\code{\link{xReduce...}}, \code{\link{xReducel}},
\code{\link{xReducel...}}
}

