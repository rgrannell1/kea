\name{xFormals}
\alias{xFormals}
\title{xFormals}
\usage{
  xFormals(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  A named list, where each element's name is a parameter in
  f and each value is the default value of that parameter.
}
\description{
  Get the parametres and defaults of a function or
  primitive function.
}
\section{Corner Cases}{
  If \code{fn} is a primitive function a heuristic is used
  to obtain its formals. If a parameter has no default, the
  value of the corresponding element in the retun value
  will be the empty symbol, which is identical to
  \code{quote(expr=)}.
}

