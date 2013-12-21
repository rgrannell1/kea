\name{xHasDefs}
\alias{xHasDefs}
\title{xHasDefs}
\usage{
xHasDefs(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
a named vector of true of false value of the same length as
the arity of \code{fn}.
}
\description{
Return a boolean vector showing which parametres of a
function has defaults.
}
\section{Corner Cases}{
  if \code{fn} is nullary \code{logical(0)} is returned.
}

