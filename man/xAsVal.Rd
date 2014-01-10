\name{xAsVal}
\alias{xAsVal}
\title{xAsVal}
\usage{
xAsVal(sym)
}
\arguments{
  \item{sym}{a symbol or string.}
}
\value{
Null; used for side-effect.
}
\description{
Convert a normal R variable to a constant value.
}
\details{
xAsVal takes a variable that exists in the calling
environment, and locks it. This prevents further
modification, and if any attempt is made to modify the
variable an error is thrown.
}
\section{Corner Cases}{
  throws an error if attempting to convert a variable that
  doesn't exist (in the parent frame).
}
\examples{

# 1. xAsVal can be used to 'lock' a list of constant variables
#    so it can't accidentally be updated.

consts <- list(
    pi = 3.14,
    golden = 1.618
)

xAsVal(consts)

# consts$pi <- 2
# would throw an error.

}
\seealso{
Other immutable_value_functions: \code{\link{xAsVar}};
\code{\link{xVal}}
}

