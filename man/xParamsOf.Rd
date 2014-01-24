\name{xParamsOf}
\alias{xParamsOf}
\title{xParamsOf}
\usage{
xParamsOf(fn)
}
\arguments{
  \item{fn}{an arbitrary function or primitive function.}
}
\value{
A character vector.
}
\description{
Get the parametre names of a function or primitive
function.
}
\section{Corner Cases}{
  If \bold{fn} is a primitive function a heuristic is used
  to obtain its parametre names, which may not work for all
  functions. If a function has no parametres the empty
  character vector is returned.
}
\seealso{
Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xFlip}};
\code{\link{xFormalsOf}}; \code{\link{xIsVariadic}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

