\name{xDo}
\alias{xDo}
\alias{xDo...}
\title{xDo}
\usage{
xDo(fn, coll)

xDo...(fn, ...)
}
\arguments{
  \item{fn}{a unary function, usually side-effectful.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Map (a possibly side-effectful) function over a collection
and discard the results.
}
\section{Uses}{
  \code{xDo} behaves similarily to \code{xMap}; it applies
  a function to every element of a collection. However,
  \code{xDo} is primarily meant for use with side-effectful
  functions, and is more memory efficient for \code{xDo}
  for this task. \code{xDo} can be used for plotting every
  point in a list of xy coordinates, or for printing every
  value in a list. It is also possible to use \code{xDo}
  for side-effectfully updating values, but this can lead
  to stateful, tangled code.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\examples{

#1. print the files in your current directory.

xDo(print, list.files('.'))

#    or

x_(list.files('.'))$xDo(print)

#    or even

xDo(print \%of\% list.files, '.')

#2. plot a trig function.

plot(
    0, 0,
    xlim = c(0, 10),
    ylim = c(-10, 10),
    type = 'n')

xDo(
    x := {

        y <- tan( sin( x ) / cos( x )^2 )
        points(x, y)

    },
    (1:1000) / 100
)
}

