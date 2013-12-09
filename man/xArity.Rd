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
  If \code{fn} has an ellipsis (...) parameter then
  \code{+Inf} is returned, as the function can accept an
  infinite number of arguments.
}
\seealso{
  Other higher_order_functions: \code{\link{xAsUnary}},
  \code{\link{xChars}}, \code{\link{xCompose}},
  \code{\link{xConst}}, \code{\link{xFlip}},
  \code{\link{xIterate}}, \code{\link{xJuxtapose}},
  \code{\link{xLimit}}, \code{\link{xNot}},
  \code{\link{xParams}}, \code{\link{xUnfold}},
  \code{\link{xWait}}
}

