\name{x_}
\alias{x_}
\title{x_}
\usage{
x_(val)
}
\arguments{
  \item{val}{a function, collection, or arbitrary value.}
}
\value{
an object of class "arrow", with a single field 'x' that
contents_are val.
}
\description{
Generate a chainable arrow object, that can use methods.
}
\details{
There are methods specific to arrow objects.

\code{xExecute}: .

\code{xGraft}: Make a new method available for use on arrow
objects.

\code{xTap}: Call the data inside the arrow object with an
anonymous function, returning a new arrow object.
}
\section{Corner Cases}{
  The methods that can be used by x_() object varies
  depending on the type of val.  Some methods are specific
  to functions or collections. If a non-function and
  non-collection is supplied then very few methods can be
  used.

  Because the definition of $ was overloaded to allow
  method chaining, the field 'x' inside an arrow object
  cannot be accessed using x_()$x. Writing x_()$x() is
  required.
}

