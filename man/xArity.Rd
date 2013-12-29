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
  If \code{fn} has an ellipsis (...) parametre then
  \code{+Inf} is returned, as the function can accept an
  infinite number of arguments.
}
\examples{

# 1. xArity works on primitive functions.

xArity('+')

2

# 2. xArity also works on normal functions
#    and variadic functions.

xArity(Reduce)

5

xArity(function (a, ...) {})

Inf

# 3. xArity can be used to check that a function
#    given to a higher-order function is safe to
#    use.

safeMap <- function (fn, coll) {

	if (!xArity(fn) == 1) {
		stop("the function given to safeMap is not unary.")
	} else {
		lapply(fn, coll)
	}
}
}
\seealso{
Other higher_order_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}, \code{\link{xCompose...}};
\code{\link{xConst}}, \code{\link{xK}}; \code{\link{xDo}},
\code{\link{xDo...}}; \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xExists}},
\code{\link{xExists...}}; \code{\link{xFilter}},
\code{\link{xFilter...}}, \code{\link{xSelect}},
\code{\link{xSelect...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xFlip}};
\code{\link{xFoldList}}, \code{\link{xFoldList...}},
\code{\link{xFoldListl}}, \code{\link{xFoldListl...}};
\code{\link{xFoldl}}; \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xForall}},
\code{\link{xForall...}}; \code{\link{xFormals}};
\code{\link{xGet}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xLimit}}; \code{\link{xLocater}},
\code{\link{xLocater...}}; \code{\link{xLocate}},
\code{\link{xLocate...}}, \code{\link{xLocatel}},
\code{\link{xLocatel...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapWhen}},
\code{\link{xMapWhen...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xPartition}};
\code{\link{xPermute}}, \code{\link{xPermute...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xPoll}}, \code{\link{xPoll...}};
\code{\link{xRecurMap}}, \code{\link{xRecurMap...}};
\code{\link{xReducer}}, \code{\link{xReducer...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}};
\code{\link{xReject}}, \code{\link{xReject...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xStopwatch}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}; \code{\link{xWait}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}}
}

