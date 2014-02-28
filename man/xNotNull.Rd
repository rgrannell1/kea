\name{xNotNull}
\alias{xNotNull}
\title{xNotNull}
\usage{
xNotNull(val)
}
\arguments{
  \item{val}{an arbitrary value. The value to test for not
  being null.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value not null?
}
\section{Corner Cases}{
  xNotNull returns either true or false, to make it safe
  for use with if statements.
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
\code{\link{xElemNotNull...}}; \code{\link{xElemNotTrue}},
\code{\link{xElemNotTrue...}}; \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotTrue}}
}

