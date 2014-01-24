\name{xThirdOf}
\alias{xThirdOf}
\alias{xThirdOf...}
\title{xThirdOf}
\usage{
xThirdOf(coll)

xThirdOf...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
The third element in \bold{coll}.
}
\description{
Return the third value in a collection.
}
\section{Corner Cases}{
  Throws an error if \bold{coll} has less than three
  elements; this is because any other corner case would
  violate the function's type-signature.
}

