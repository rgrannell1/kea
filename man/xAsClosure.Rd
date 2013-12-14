\name{xAsClosure}
\alias{xAsClosure}
\title{xAsClosure}
\usage{
xAsClosure(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
a function (closure).
}
\description{
Convert a primitive function to a closure.
}
\section{Corner Cases}{
  xAsClosure does not work for every primitive function
  (for example 'c'), so caution should be taken when using
  this function.
}

