\name{xSignum}
\alias{xSignum}
\alias{xSignum...}
\title{xSignum}
\usage{
xSignum(nums)

xSignum...(...)
}
\arguments{
  \item{nums}{a vector of non-complex numbers.}

  \item{...}{see above.}
}
\value{
a integer vector of elements in the set \code{{-1, 0, +1}}.
}
\description{
Get the sign of a vector of numbers.
}
\section{Corner Cases}{
  If \code{nums} is lenth-zero then the unit of that vector
  is returned.  The sign of zero is zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}

