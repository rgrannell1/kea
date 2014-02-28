\name{x_}
\alias{x_}
\title{x_}
\usage{
x_(val)
}
\arguments{
  \item{val}{an arbitrary value. The value to wrap in an
  arrow object.  The methods available depend on the input
  type; functions and collections have the most methods
  available.}
}
\value{
An object of class "arrow". Internally the object is
represented as a list with a single field \bold{x}, but
this field cannot be accessed directly.  Instead, the
method \bold{$ x_( )} or \bold{$ x_Identity( )} can be used
to return the data stored in an arrow object.

The methods available to an arrow object depend on the type
of the data it contains. All arrow objects inherit a
handful of methods regardless of their type; these include
\bold{xIdentity} and \bold{xTap} - a method that allows
anonymous function to be executed on an arrow object.

Matrices, data frames, and factors have methods for
converting them to collections, while normal Arrow
functions are also available as methods for collections and
functions.
}
\description{
Generate an arrow object with methods available to it.
}
\section{Corner Cases}{
  The methods that can be used by \bold{$ x_( )} object
  varies depending on the type of val.
}
\examples{

Null
}
\seealso{
Other methods: \code{\link{xByColnames}};
\code{\link{xByCols}}; \code{\link{xByLevels}},
\code{\link{xByLevels}}; \code{\link{xByRows}};
\code{\link{xColUnit}}; \code{\link{xElemsByCols}};
\code{\link{xElemsByRows}}; \code{\link{xExecute}};
\code{\link{xRowUnit}}; \code{\link{xTap}};
\code{\link{xTranspose}}
}

