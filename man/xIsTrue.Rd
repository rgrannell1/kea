\name{xIsTrue}
\alias{xIsTrue}
\title{xIsTrue}
\usage{
xIsTrue(val)
}
\arguments{
  \item{val}{an arbitrary value.}

  \item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value true?
}
\section{Corner Cases}{
  xIsTrue returns either true or false, to make it safe for
  use with if statements. If val is length zero False is
  returned.
}
\examples{

NULL
}
\seealso{
Other testing_functions: \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotNull}}; \code{\link{xNotTrue}}
}

