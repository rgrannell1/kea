\name{xLambda}
\alias{:=}
\alias{xLambda}
\title{xLambda}
\usage{
xLambda(formals, body)

":="(formals, body)
}
\arguments{
  \item{formals}{a symbol or string.}

  \item{body}{a valid function body, which will be lazily
  evaluated.}
}
\value{
returns a unary function.
}
\description{
Syntactic sugar for creating unary functions.
}

