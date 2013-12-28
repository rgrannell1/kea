\name{xVal}
\alias{xVal}
\title{xVal}
\usage{
xVal(str, val)
}
\arguments{
  \item{str}{a string or symbol.}

  \item{val}{an arbitrary value.}
}
\value{
Null; this function is used for side-effects.
}
\description{
Assign a constant value to calling environment.
}
\section{Corner Cases}{
  overwrites the value referenced by \code{str}r if the
  variable already exists in the parent frame.
}
\seealso{
Other immutable_value_functions: \code{\link{xAsVal}};
\code{\link{xAsVar}}
}

