\name{xParams}
\alias{xParams}
\title{xParams}
\usage{
  xParams(fn)
}
\arguments{
  \item{fn}{an arbitrary function or primitive function.}
}
\value{
  a character vector/.
}
\description{
  Get the parametre names of a function or primitive
  function.
}
\section{Corner Cases}{
  If \code{fn} is a primitive function a heuristic is used
  to obtain its parameter names.
}
\seealso{
  Other higher_order_functions: \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xChars}},
  \code{\link{xCompose}}, \code{\link{xConst}},
  \code{\link{xFlip}}, \code{\link{xIterate}},
  \code{\link{xJuxtapose}}, \code{\link{xLimit}},
  \code{\link{xNot}}, \code{\link{xUnfold}},
  \code{\link{xWait}}
}

