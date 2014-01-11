\name{xThird}
\alias{xThird}
\alias{xThird...}
\title{xThird}
\usage{
xThird(coll)

xThird...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
The third element in \code{coll}.
}
\description{
Return the third value in a collection.
}
\section{Corner Cases}{
  throws an error if \code{coll} has less than three
  elements; this is because any other corner case would
  violate the function's type-signature.
}

\section{Variadic Functions

\link{ help_variadic }}{

}

