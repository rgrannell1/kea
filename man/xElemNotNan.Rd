\name{xElemNotNan}
\alias{xElemNotNan}
\alias{xElemNotNan...}
\title{xElemNotNan}
\usage{
xElemNotNan(coll)

xElemNotNan...(...)
}
\arguments{
  \item{coll}{a collection. The collection to test each
  element of for being non-nan.}

  \item{...}{see above.}
}
\value{
A vector of boolean values.
}
\description{
Is an element of a collection not NaN?
}
\section{Corner Cases}{
  Returns the empty list if \code{coll is length-zero}.
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
\code{\link{xElemIsTrue...}}; \code{\link{xElemNotFalse}},
\code{\link{xElemNotFalse...}}; \code{\link{xElemNotNa}},
\code{\link{xElemNotNa...}}; \code{\link{xElemNotNull}},
\code{\link{xElemNotNull...}}; \code{\link{xElemNotTrue}},
\code{\link{xElemNotTrue...}}; \code{\link{xIsEmpty}},
\code{\link{xIsEmpty...}}; \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xNotEmpty}}, \code{\link{xNotEmpty...}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotNull}};
\code{\link{xNotTrue}}
}

