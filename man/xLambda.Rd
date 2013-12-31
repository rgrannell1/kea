\name{xLambda}
\alias{:=}
\alias{xLambda}
\title{xLambda}
\usage{
xLambda(sym, val)

":="(sym, val)
}
\arguments{
  \item{sym}{a symbol or string.}

  \item{val}{a valid function body, which will be lazily
  evaluated.}
}
\value{
returns a unary function.
}
\description{
Syntactic sugar for creating unary functions.
}

