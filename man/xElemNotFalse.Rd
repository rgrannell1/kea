\name{xElemNotFalse}
\alias{xElemNotFalse}
\alias{xElemNotFalse...}
\title{xElemNotFalse}
\usage{
xElemNotFalse(coll)

xElemNotFalse...(...)
}
\arguments{
  \item{coll}{a collection. The collection to test each
  element of for being non-false.}

  \item{...}{see above.}
}
\value{
A vector of boolean values.
}
\description{
Is an element of a collection not false?
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

NULL

}
\seealso{
Other value_testing_functions: \code{\link{xElemIsFalse}},
\code{\link{xElemIsFalse...}}; \code{\link{xElemIsNan}},
\code{\link{xElemIsNan...}}; \code{\link{xElemIsNa}},
\code{\link{xElemIsNa...}}; \code{\link{xElemIsNull}},
\code{\link{xElemIsNull...}}; \code{\link{xElemIsTrue}},
\code{\link{xElemIsTrue...}}; \code{\link{xElemNotNan}},
\code{\link{xElemNotNan...}}; \code{\link{xElemNotNa}},
\code{\link{xElemNotNa...}}; \code{\link{xElemNotNull}},
\code{\link{xElemNotNull...}}; \code{\link{xElemNotTrue}},
\code{\link{xElemNotTrue...}}; \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotNull}};
\code{\link{xNotTrue}}
}

