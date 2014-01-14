\name{xReducer}
\alias{xReducer}
\alias{xReducer...}
\title{xReducer}
\usage{
xReducer(fn, coll)

xReducer...(fn, ...)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its left argument.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
An arbitrary value, depending on the function \code{fn}.
}
\description{
Successively combine a list of values into a single value
using a binary function (right to left).
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero, and
  returns the value inside \code{coll} if coll is
  length-one.
}

\section{Variadic Functions

\link{ help_variadic }}{

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
\seealso{
Other folding_functions: \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xFold}},
\code{\link{xFoldl}}, \code{\link{xFoldl...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}};
\code{\link{xScan}}, \code{\link{xScan...}},
\code{\link{xScanl}}, \code{\link{xScanl...}}
}

