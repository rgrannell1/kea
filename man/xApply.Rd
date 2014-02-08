\name{xApply}
\alias{xApply}
\alias{xApply...}
\title{xApply}
\usage{
xApply(fn, coll)

xApply...(fn, ...)
}
\arguments{
  \item{fn}{an function of any arity. The function to
  call.}

  \item{coll}{a collection. The arguments to pass to
  \bold{fn}. The collection may be named or unnamed; normal
  R function call semantics dictate how the arguments are
  used by the input function.}

  \item{...}{see above.}
}
\value{
The return value of \bold{fn}.
}
\description{
Invoke a function with a collection of arguments.
}
\details{
\bold{xApply} is an adaptor function that allows any
function to be called with a collection of arguments. A
typical use case is to allow a function with ellipsis (...)
arguments to be called with a variable number of arguments.

\code{coll <- list( list(1, 2), list(3, 4), list(5, 6) )}

\code{xApply(rbind, coll)}

In the above case the function rbind is called on several
rows, without having to use reduce or a for loop to
successively bind the rows.

It isn't usually necessary to call arrow functions with
\bold{xApply}, since they exist with both variadic and
non-variadic forms.
}
\examples{

# 1. call a variadic function with xApply

nums <- c(1, 5,2,3)

xApply(sum, nums)

# 11

# normally, variadic functions have to be called with
# a fixed number of arguments, as in

a = 1; b = 5; c = 2; d = 3

sum(a, b, c, d)

# 11

# which cannot be changed at runtime.

# 2. Reduce and xApply can often be used to solve the
#    same problems.

xReduce('+', 1:10)

# 55

xApply(sum, 1:10)

# 55

# 3. Rbind and xApply can be used to build up a matrix
#    from vectors.

xApply...(rbind, list(1, 2), list(3, 4), list(5, 6))

# structure(
#     list(1, 3, 5, 2, 4, 6),
#     .Dim = c(3L, 2L))
}
\seealso{
Other function_application_functions:
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xThread}}, \code{\link{xThread...}}
}

