\name{xAsVar}
\alias{xAsVar}
\title{xAsVar}
\usage{
xAsVar(str)
}
\arguments{
  \item{str}{a string or symbol.}
}
\value{
Null; used for side-effect.
}
\description{
Convert a constant value back into a normal R value.
}
\section{Corner Cases}{
  throws an error if attempting to convert a variable that
  doesn't exist (in the parent frame).
}
\examples{

# 1. xAsVar can be used to unlock values back to
#    variables, since every function needs an inverse.

const <- list(
    letters =
        letters
)

xAsVal(const)
xAsVar(const)

const <- NULL
const

NULL
}
\seealso{
Other immutable_value_functions: \code{\link{xAsVal}};
\code{\link{xVal}}
}

