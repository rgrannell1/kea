\name{xIsFalse}
\alias{xIsFalse}
\title{xIsFalse}
\usage{
xIsFalse(val)
}
\arguments{
  \item{val}{an arbitrary value. The value to test for
  being false.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value false?
}
\section{Corner Cases}{
  xIsFalse returns either true or false, to make it safe
  for use with if statements. If val is length zero False
  is returned.
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
\code{\link{xElemNotTrue...}}; \code{\link{xIsEmpty}},
\code{\link{xIsEmpty...}}; \code{\link{xIsNan}};
\code{\link{xIsNa}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xNotEmpty}},
\code{\link{xNotEmpty...}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotNull}}; \code{\link{xNotTrue}}
}

