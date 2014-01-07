\name{xAsNamed}
\alias{xAsNamed}
\title{xAsNamed}
\usage{
xAsNamed(strs, coll)
}
\arguments{
  \item{strs}{a collection of strings.}

  \item{coll}{a collection}
}
\value{
a named list.
}
\description{
Add names to a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other name_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xGetKey}};
\code{\link{xPluck}}, \code{\link{xPluck...}}
}

