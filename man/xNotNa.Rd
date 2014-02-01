\name{xNotNa}
\alias{xNotNa}
\title{xNotNa}
\usage{
xNotNa(val)
}
\arguments{
  \item{val}{an arbitrary value.The value to test for
  not-na.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value not na?
}
\section{Corner Cases}{
  xNotNa returns either true or false, to make it safe for
  use with if statements. If val is length zero True is
  returned.
}
\examples{

NULL
}
\seealso{
Other testing_functions: \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNull}}; \code{\link{xNotTrue}}
}

