\name{xScanl}
\alias{xScanl}
\title{xScanl}
\usage{
  xScanl(fn, init, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its right argument.}

  \item{init}{an arbitrary value.}

  \item{coll}{a collection.}
}
\value{
  a list with its init element being \code{coll}, and
  containing \code{length(coll) + 1}.
}
\description{
  Fold a function over a collection from left to right with
  an initital left value, keeping intermediate values.
}
\section{Corner Cases}{
  returns \code{list(init)} if \code{coll} is length-zero.
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
\seealso{
  Other higher_order_function: \code{\link{xAndLift}},
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
  \code{\link{xMinusLift}}, \code{\link{xModLift}},
  \code{\link{xNot}}, \code{\link{xOrLift}},
  \code{\link{xOverLift}}, \code{\link{xParams}},
  \code{\link{xPartMap}}, \code{\link{xPartial}},
  \code{\link{xPartition}}, \code{\link{xPartitionWith}},
  \code{\link{xPluck}}, \code{\link{xPlusLift}},
  \code{\link{xPoll}}, \code{\link{xRecurMap}},
  \code{\link{xReduce}}, \code{\link{xReducer}},
  \code{\link{xReject}}, \code{\link{xSelect}},
  \code{\link{xSplitWith}}, \code{\link{xSwap}},
  \code{\link{xTakeWhile}}, \code{\link{xUnfold}},
  \code{\link{xUnionLift}}, \code{\link{xUntil}},
  \code{\link{xWait}}, \code{\link{xWhile}},
  \code{\link{xWrap}}, \code{\link{xZipWith}}
}

