\name{xFormals}
\alias{xFormals}
\title{xFormals}
\usage{
xFormals(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
A named list, where each element's name is a parametre in f
and each value is the default value of that parametre.
}
\description{
Get the parametres and defaults of a function or primitive
function.
}
\section{Corner Cases}{
  If \code{fn} is a primitive function a heuristic is used
  to obtain its formals. If a parametre has no default, the
  value of the corresponding element in the retun value
  will be the empty symbol, which is identical to
  \code{quote(expr=)}.
}
\seealso{
Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsUnary}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xFlip}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xLimit}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xStopWatch}};
\code{\link{xUnfold}}; \code{\link{xWait}}
}

