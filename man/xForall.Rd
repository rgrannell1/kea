\name{xForall}
\alias{xForall}
\alias{xForall...}
\title{xForall}
\usage{
xForall(pred, colls)

xForall...(pred, ...)
}
\arguments{
  \item{pred}{an n-ary predicate.}

  \item{colls}{n collections.}

  \item{...}{see above.}
}
\value{
a boolean value.
}
\description{
Does ever selection of arguments satisfy a predicate?
}
\section{Corner Cases}{
  if any collection is length zero (or no collections are
  given), then False is returned.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other quantifier_functions: \code{\link{xExists}},
\code{\link{xExists...}}
}

