\name{xAsVal}
\alias{xAsVal}
\title{xAsVal}
\usage{
xAsVal(str)
}
\arguments{
  \item{str}{a string or symbol.}
}
\value{
Null; used for side-effect.
}
\description{
Convert a normal R variable to a constant value.
}
\section{Corner Cases}{
  throws an error if attempting to convert a variable that
  doesn't exist (in the parent frame).
}

