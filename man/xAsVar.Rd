\name{xAsVar}
\alias{xAsVar}
\title{xAsVar}
\usage{
xAsVar(sym)
}
\arguments{
  \item{sym}{a symbol or string. The name of the variable
  to be converted to a variable.}
}
\value{
Null; used for side-effect.
}
\description{
Convert a constant value back into a normal R value.
}
\details{
\code{xAsVar} takes a variable in the calling environmnet,
and unlocks it, converting it back to a normal R variable.
}
\section{Corner Cases}{
  Throws an error if attempting to convert a variable that
  doesn't exist (in the parent frame).  Non-locked
  variables are also allowed.
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

