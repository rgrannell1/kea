\name{xTranspose}
\alias{xTranspose}
\title{$ xTranspose}
\usage{
x_(  ) $ xTranspose()
}
\value{
An arrow object containing a matrix.
}
\description{
Transpose a matrix.
}
\details{
\bold{xTranspose( )} is very similar to the base function
\bold{t( )}. It takes each row of an input matrix and uses
it as a column in the output matrix. The dimensions of the
input matrix is flipped; if a 4 x 2 matrix is supplied to
\bold{xTranspose( )} then a 2 x 4 matrix is returned.

\bold{xTranspose} is an involution - a function that is its
own inverse. For any collection,
\bold{xTranspose(xTranspose(coll))} is the original
collection.
}
\section{Corner Cases}{
  The empty matrix is returned if the input matrix is
  empty.  Matrices with zero rows or columns are transposed
  as expected.
}
\seealso{
Other methods: \code{\link{xByColnames}};
\code{\link{xByCols}}; \code{\link{xByLevels}},
\code{\link{xByLevels}}; \code{\link{xByRows}};
\code{\link{xColUnit}}; \code{\link{xElemsByCols}};
\code{\link{xElemsByRows}}; \code{\link{xExecute}};
\code{\link{xRowUnit}}; \code{\link{xTap}};
\code{\link{x_}}
}

