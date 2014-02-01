\name{xNotFalse}
\alias{xNotFalse}
\title{xNotFalse}
\usage{
xNotFalse(val)
}
\arguments{
  \item{val}{an arbitrary value.The value to test for
  not-falsity.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value not sfalse?
}
\section{Corner Cases}{
  xNotFalse returns either true or false, to make it safe
  for use with if statements. If val is length zero True is
  returned.
}
\examples{

NULL
}
\seealso{
Other testing_functions: \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotNull}}; \code{\link{xNotTrue}}
}

