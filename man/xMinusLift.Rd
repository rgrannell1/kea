\name{xMinusLift}
\alias{xMinusLift}
\title{xMinusLift}
\usage{
  xMinusLift(fn1, fn2)
}
\arguments{
  \item{fn1}{a function that returns a number.}

  \item{fn2}{a function that returns a number.}
}
\value{
  a unary function of x.
}
\description{
  Compose two function with subtraction
}
\section{Glossary}{
  \bold{Function}: a closure or primitive function.  A
  symbol or string refering to such a function may also be
  used.

  \bold{Predicate}: A function that returns a logical value
  (unary, unless stated otherwise).  A symbol or string
  refering to such a function may also be used.

  \bold{Collection}: A list, pairlist or a typed vector.

  \bold{Arity}: The number of arguments a function can
  accept.

  \bold{Nullary}: a function with arity zero. \bold{Unary}:
  a function with arity one. \bold{Binary}: a function with
  arity two. \bold{Trinary}: a function with arity three.

  \bold{Number}: a double, integer or complex number,
  unless stated otherwise.

  \bold{Higher Order Function}: a function that takes a
  function as one of its arguments, and/or returns a
  function.
}
\examples{
inst/examples/blank.R
}
\seealso{
  Other function_lifting: \code{\link{xAndLift}},
  \code{\link{xByLift}}, \code{\link{xEqualLift}},
  \code{\link{xInterLift}}, \code{\link{xModLift}},
  \code{\link{xOrLift}}, \code{\link{xOverLift}},
  \code{\link{xPlusLift}}, \code{\link{xUnionLift}}

  Other higher_order_functions: \code{\link{xAndLift}},
  \code{\link{xApply}}, \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xByLift}},
  \code{\link{xChars}}, \code{\link{xCompose}},
  \code{\link{xConst}}, \code{\link{xDifferLift}},
  \code{\link{xDo}}, \code{\link{xDropWhile}},
  \code{\link{xEqualLift}}, \code{\link{xExists}},
  \code{\link{xFixDefs}}, \code{\link{xFlatMap}},
  \code{\link{xFlip}}, \code{\link{xFoldl}},
  \code{\link{xForall}}, \code{\link{xInterLift}},
  \code{\link{xJuxtapose}}, \code{\link{xLimit}},
  \code{\link{xLocate}}, \code{\link{xLocater}},
  \code{\link{xMap}}, \code{\link{xMapAlong}},
  \code{\link{xMapMany}}, \code{\link{xMapWhen}},
  \code{\link{xModLift}}, \code{\link{xNot}},
  \code{\link{xOrLift}}, \code{\link{xOverLift}},
  \code{\link{xParams}}, \code{\link{xPartMap}},
  \code{\link{xPartial}}, \code{\link{xPartition}},
  \code{\link{xPartitionWith}}, \code{\link{xPluck}},
  \code{\link{xPlusLift}}, \code{\link{xPoll}},
  \code{\link{xRecurMap}}, \code{\link{xReduce}},
  \code{\link{xReducer}}, \code{\link{xReject}},
  \code{\link{xScanl}}, \code{\link{xSelect}},
  \code{\link{xSplitWith}}, \code{\link{xSwap}},
  \code{\link{xTakeWhile}}, \code{\link{xUnfold}},
  \code{\link{xUnionLift}}, \code{\link{xUntil}},
  \code{\link{xWait}}, \code{\link{xWhile}},
  \code{\link{xWrap}}, \code{\link{xZipWith}}
}

