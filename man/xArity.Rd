\name{xArity}
\alias{xArity}
\title{xArity}
\usage{
xArity(fn)
}
\arguments{
  \item{fn}{an function of any arity.}
}
\value{
a positive whole number.
}
\description{
Return the number of arguments a function can accept.
}
\section{Uses}{
  \code{xArity} is primarily intended for use with
  higher-order functions that require functions with a
  particular arity. For example, a strict version of map
  might use \code{xArity} to throw an error if its input
  function is not unary.
}

\section{Corner Cases}{
  If \code{fn} has an ellipsis (...) parametre then
  \code{+Inf} is returned, as the function can accept an
  infinite number of arguments.
}
\examples{

# 1. xArity works on primitive functions.

xArity('+')

2

# 2. xArity also works on normal functions
#    and variadic functions.

xArity(Reduce)

5

xArity(function (a, ...) {})

Inf

# 3. xArity can be used to check that a function
#    given to a higher-order function is safe to
#    use.

safeMap <- function (fn, coll) {

    if (!xArity(fn) == 1) {
        stop("the function given to safeMap is not unary.")
    } else {
        lapply(fn, coll)
    }
}
}

