\name{xNotNa}
\alias{xNotNa}
\alias{xNotNa...}
\title{xNotNa}
\usage{
xNotNa(coll)

xNotNa...(...)
}
\arguments{
  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A vector of true/false values.
}
\description{
Test every element in a collection for being \code{Na} of
any type.
}
\section{Corner Cases}{
  Returns logical(0) if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}

