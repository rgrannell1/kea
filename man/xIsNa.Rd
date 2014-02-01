\name{xIsNa}
\alias{xIsNa}
\title{xIsNa}
\usage{
xIsNa(val)
}
\arguments{
  \item{val}{an arbitrary value. The value to test for
  being na.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value na?
}
\section{Corner Cases}{
  xIsNa returns either true or false, to make it safe for
  use with if statements. If val is length zero False is
  returned.
}
\examples{

NULL
}
\seealso{
Other testing_functions: \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotNull}}; \code{\link{xNotTrue}}
}

