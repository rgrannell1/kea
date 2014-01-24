\name{xPartial}
\alias{xPartial}
\alias{xPartial...}
\title{xPartial}
\usage{
xPartial(fn, coll)

xPartial...(fn, ...)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A function of equal or lesser arity to \bold{fn}.
}
\description{
Partially apply a function.
}
\section{Corner Cases}{
  Partial application also works for ellipses (eg list(...
  = 1)).
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}

Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xFlip}};
\code{\link{xFormalsOf}}; \code{\link{xIsVariadic}};
\code{\link{xParamsOf}}
}

