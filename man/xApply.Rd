\name{xApply}
\alias{xApply}
\title{xApply}
\usage{
xApply(fn, coll)
}
\arguments{
  \item{fn}{an function of any arity.}

  \item{coll}{a list or pairlist. This may be named, but
  all names must be parameters of \code{fn}.}
}
\value{
the return value of \code{fn}.
}
\description{
Call a function with a list of arguments.
}
\section{Uses}{
  \code{xApply} is an adaptor function that allows variadic
  functions to be called with a variable number of
  arguments at runtime.  For example, if \code{xConcat} is
  invoked directly it has to be given a fixed number of
  lists to concatenate.  If \code{xConcat} is invoked with
  \code{xApply} then xConcat can be invoked with any number
  of lists dynamically.
}
\seealso{
Other collection_functions: \code{\link{xAsFunction}};
\code{\link{xAssoc}}; \code{\link{xIsMember}};
\code{\link{xDiffer}}; \code{\link{xDissoc}};
\code{\link{xDrop}}; \code{\link{xFirst}};
\code{\link{xFlatten}}; \code{\link{xFourth}};
\code{\link{xInit}}; \code{\link{xInter}};
\code{\link{xIsEmpty}}; \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xLast}}; \code{\link{xLength}};
\code{\link{xName}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotTrue}}; \code{\link{xPack}};
\code{\link{xRepeat}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xShuffle}};
\code{\link{xSplit}}; \code{\link{xTake}};
\code{\link{xThird}}; \code{\link{xUnion}};
\code{\link{xUnit}}

Other higher_order_functions: \code{\link{xArity}};
\code{\link{xAsUnary}}; \code{\link{xCompose}};
\code{\link{xConst}}; \code{\link{xFlip}};
\code{\link{xFormals}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xLimit}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xStopWatch}};
\code{\link{xUnfold}}; \code{\link{xWait}}
}

