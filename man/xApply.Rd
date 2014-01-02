\name{xApply}
\alias{xApply}
\alias{xApply...}
\title{xApply}
\usage{
xApply(fn, coll)

xApply...(fn, ...)
}
\arguments{
  \item{fn}{an function of any arity.}

  \item{coll}{a list or pairlist. This may be named, but
  all names must be parametres of \code{fn}.}

  \item{...}{see above.}
}
\value{
the return value of \code{fn}.
}
\description{
Invoke a function with a collection of arguments.
}
\section{Uses}{
  \code{xApply} is an adaptor function that allows variadic
  functions to be called with a variable number of
  arguments at runtime.  For example, if \code{xJoin} is
  invoked directly it has to be given a fixed number of
  lists to concatenate.  If \code{xJoin} is invoked with
  \code{xApply} then xJoin can be invoked with any number
  of lists dynamically.
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

  \code{ xMap...( xGetKey('a'), list(a = 1, b = 1), list(a
  = 2, b = 1))}

  instead of

  \code{ xMap( xGetKey('a'), list( list(a = 1, b = 1),
  list(a = 2, b = 1)) )}

  For other applications the xMethod form of the function
  is more likely to be useful:

  \code{nums <- (1:10) / 10}

  \code{xMap(sqrt, nums)}

  As opposed to

  \code{xApply(xMap..., list(sqrt, nums))}

  Arrow methods also have variadic and non-variadic forms
  in many cases.
}

\section{Appendix-Glossary}{
  In order to keep the documentation for what is a very
  large library terse, certain words are taken to have
  precise definitions, unless defined otherwise.

  \bold{Arity}: The number of arguments a function can
  accept.

  \bold{Binary}: a function with arity two.

  \bold{Collection}: A list, pairlist or a typed vector.

  \bold{Function}: a closure or primitive function.  A
  symbol or string refering to such a function may also be
  used.

  \bold{Higher Order Function}: a function that takes a
  function as one of its arguments, and/or returns a
  function.

  \bold{Nullary}: a function with arity zero.

  \bold{Number}: a double, integer or complex number,
  unless stated otherwise.

  \bold{Predicate}: A function that returns a logical value
  (unary, unless stated otherwise).  A symbol or string
  refering to such a function may also be used.

  \bold{Trinary, Ternary}: a function with arity three.

  \bold{Unary}: a function with arity one.

  \bold{Variadic}: a function that can accept any infinite
  number of arguments.
}
\examples{

# 1. call a variadic function with xApply

nums <- c(1, 5,2,3)

xApply(sum, nums)

11

# normally, variadic functions have to be called with
# a fixed number of arguments, as in

a = 1; b = 5; c = 2; d = 3

sum(a, b, c, d)

11

# which cannot be changed at runtime.

# 2. Reduce and xApply can often be used to solve the
#    same problems.

xReduce('+', 1:10)

55

xApply(sum, 1:10)

55

# 3. Rbind and xApply can be used to build up a matrix
#    from vectors.

xApply...(rbind, list(1, 2), list(3, 4), list(5, 6))

structure(
    list(1, 3, 5, 2, 4, 6),
    .Dim = c(3L, 2L))
}
\seealso{
Other function_application_functions:
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xThread}}, \code{\link{xThread...}}
}

