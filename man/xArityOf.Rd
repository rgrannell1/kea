\name{xArityOf}
\alias{xArityOf}
\title{xArityOf}
\usage{
xArityOf(fn)
}
\arguments{
  \item{fn}{an function. The function to have its arity
  checked.}
}
\value{
A positive whole number.
}
\description{
Return the number of arguments a function can accept.
}
\details{
\bold{xArityOf} returns the arity of a function - the
number of arguments that can be passed to the function. R
functions can be nullary (0-arguments), have infinitely
many arguments (variadic functions), or have a fixed number
of arguments (non-variadic functions).
}
\section{Corner Cases}{
  If \bold{fn} has an ellipsis (...) parametre then
  \code{+Inf} is returned, as the function can accept an
  infinite number of arguments.
}
\examples{

# 1. xArityOf works on primitive functions.

xArityOf('+')

# 2

# 2. xArityOf also works on normal functions
#    and variadic functions.

xArityOf(Reduce)

# 5

xArityOf(function (a, ...) {})

# Inf

# 3. xArityOf can be used to check that a function
#    given to a higher-order function is safe to
#    use.

safeMap <- function (fn, coll) {

    if (!xArityOf(fn) == 1) {
        stop("the function given to safeMap is not unary.")
    } else {
        lapply(fn, coll)
    }
}
}
\seealso{
Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xFormalsOf}};
\code{\link{xIsVariadic}}; \code{\link{xParamsOf}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

