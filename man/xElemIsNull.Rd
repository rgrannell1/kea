\name{xElemIsNull}
\alias{xElemIsNull}
\alias{xElemIsNull...}
\title{xElemIsNull}
\usage{
xElemIsNull(coll)

xElemIsNull...(...)
}
\arguments{
  \item{coll}{a collection. The collection to test each
  element of for being null.}

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

