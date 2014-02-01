\name{xElemNotNull}
\alias{xElemNotNull}
\alias{xElemNotNull...}
\title{xElemNotNull}
\usage{
xElemNotNull(coll)

xElemNotNull...(...)
}
\arguments{
  \item{coll}{a collection. The collection to test each
  element of for being non-null.}

  \item{...}{see above.}
}
\value{
A vector of boolean values.
}
\description{
Is an element of a collection null?
}
\section{Corner Cases}{
  Returns logical(0) if coll is itself Null.
}
\examples{

NULL

}

