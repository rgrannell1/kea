\name{xFoldListl}
\alias{xFoldList}
\alias{xFoldList...}
\alias{xFoldListl}
\alias{xFoldListl...}
\title{xFoldListl}
\usage{
xFoldListl(fn, val, coll)

xFoldList(fn, val, coll)

xFoldListl...(fn, val, ...)

xFoldList...(fn, val, ...)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its right argument.}

  \item{val}{an arbitrary value.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list with its initial element being \code{coll}, and
containing \code{length(coll) + 1}.
}
\description{
Successively combine a list of values into a single value
using a binary function (left to right, with an initial
value). Return a list containing each intermediate result,
and the final result.
}
\section{Corner Cases}{
  returns \code{list(val)} if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other folding_functions: \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xFold}},
\code{\link{xFoldl}}, \code{\link{xFoldl...}};
\code{\link{xReducer}}, \code{\link{xReducer...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}}
}

