\name{xFormalsOf}
\alias{xFormalsOf}
\title{xFormalsOf}
\usage{
xFormalsOf(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
A named list, where each element's name is a parametre in
\bold{fn} and each value is the default value of that
parametre.
}
\description{
Get the parametres and defaults of a function or primitive
function.
}
\section{Corner Cases}{
  If \bold{fn} is a primitive function a heuristic is used
  to obtain its formals. If a parametre has no default, the
  value of the corresponding element in the retun value
  will be the empty symbol, which is identical to
  \code{quote(expr=)}.
}
\seealso{
Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xFlip}};
\code{\link{xIsVariadic}}; \code{\link{xParamsOf}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

