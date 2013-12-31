\name{xVal}
\alias{xVal}
\title{xVal}
\usage{
xVal(sym, val)
}
\arguments{
  \item{sym}{a symbol or string.}

  \item{val}{an arbitrary value.}
}
\value{
Null; this function is used for side-effects.
}
\description{
Assign a constant value to calling environment.
}
\section{Corner Cases}{
  overwrites the value referenced by \code{sym}r if the
  variable already exists in the parent frame.
}
\seealso{
Other immutable_value_functions: \code{\link{xAsVal}};
\code{\link{xAsVar}}
}

