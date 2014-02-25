\name{xIdentity}
\alias{xI}
\alias{xIdentity}
\title{xIdentity}
\usage{
xIdentity(val)

xI(val)
}
\arguments{
  \item{val}{an arbitrary value.}
}
\value{
Returns \code{val}.
}
\description{
Return an argument without modification.
}
\examples{

all <- xPartial(xForall, list(fn = xIdentity))
all(c(True, False, True))

# False
}

