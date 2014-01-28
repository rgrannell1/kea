\name{xNotNull}
\alias{xNotNull}
\title{xNotNull}
\usage{
xNotNull(val)
}
\arguments{
  \item{val}{an arbitrary value.}

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
Other testing_functions: \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotTrue}}
}

