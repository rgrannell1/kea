\name{xNotNan}
\alias{xNotNan}
\title{xNotNan}
\usage{
xNotNan(val)
}
\arguments{
  \item{val}{an arbitrary value.The value to test for
  not-nan.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value not nan?
}
\section{Corner Cases}{
  xNotNan returns either true or false, to make it safe for
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
\code{\link{xNotFalse}}; \code{\link{xNotNa}};
\code{\link{xNotNull}}; \code{\link{xNotTrue}}
}

