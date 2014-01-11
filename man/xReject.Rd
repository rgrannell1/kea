\name{xReject}
\alias{xReject}
\alias{xReject...}
\title{xReject}
\usage{
xReject(pred, coll)

xReject...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Remove all elements from a collection matching a predicate.
}
\section{Uses}{
  The uses for the reject function are very similar to
  those for select.
}

\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero.
  Throws an error if pred returns a non-boolean value.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other filtering_functions: \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartition}};
\code{\link{xSelect}}, \code{\link{xSelect...}}
}

