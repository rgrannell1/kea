\name{xExists}
\alias{xExists}
\alias{xExists...}
\title{xExists}
\usage{
xExists(pred, colls)

xExists...(pred, ...)
}
\arguments{
  \item{pred}{an n-ary predicate.}

  \item{colls}{n collections.}

  \item{...}{see above.}
}
\value{
A boolean value.
}
\description{
Does any selection of arguments satisfy a predicate?
}
\section{Corner Cases}{
  if any collection is length zero (or no collections are
  given), then False is returned.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other quantifier_functions: \code{\link{xForall}},
\code{\link{xForall...}}
}

