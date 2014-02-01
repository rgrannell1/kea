\name{xIsNull}
\alias{xIsNull}
\title{xIsNull}
\usage{
xIsNull(val)
}
\arguments{
  \item{val}{an arbitrary value. The value to test for
  being null.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value null?
}
\section{Corner Cases}{
  xIsNull returns either true or false, to make it safe for
  use with if statements.
}
\examples{

NULL
}
\seealso{
Other testing_functions: \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsTrue}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotNull}}; \code{\link{xNotTrue}}
}

