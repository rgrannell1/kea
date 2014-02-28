\name{xElemNotTrue}
\alias{xElemNotTrue}
\alias{xElemNotTrue...}
\title{xElemNotTrue}
\usage{
xElemNotTrue(coll)

xElemNotTrue...(...)
}
\arguments{
  \item{coll}{a collection. The collection to test each
  element of for being non-true.}

  \item{...}{see above.}
}
\value{
A vector of boolean values.
}
\description{
Is an element of a collection not true?
}
\section{Corner Cases}{
  Returns logical(0) if \bold{coll} is length-zero.
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
\code{\link{xElemNotFalse...}}; \code{\link{xElemNotNan}},
\code{\link{xElemNotNan...}}; \code{\link{xElemNotNa}},
\code{\link{xElemNotNa...}}; \code{\link{xElemNotNull}},
\code{\link{xElemNotNull...}}; \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotNull}};
\code{\link{xNotTrue}}
}

