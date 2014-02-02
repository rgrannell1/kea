\name{xLambda}
\alias{:=}
\alias{xLambda}
\title{xLambda}
\usage{
xLambda(sym, val)

":="(sym, val)
}
\arguments{
  \item{sym}{a parametres body.}

  \item{val}{a valid function body, which will be lazily
  evaluated.}
}
\value{
Returns a unary function.
}
\description{
Syntactic sugar for creating unary functions.
}
\examples{

# 1. greatest common divisor:
#    a multi parametre function.

gcd <- (a : b) := {
    if (b == 0) a else gcd(b, a \%\% b)
}

gcd(156, 87)

# 3

}

