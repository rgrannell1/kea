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
  contains val.
}
\description{
  Generate a chainable arrow object, that can use methods.
}
\details{
  Creating arrow objects is efficient, since no methods are
  copied on instantiation. Invoking an arrow method (using
  $) has a small amount overhead, since the definition of $
  has been overloading to allow method calling.
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

\section{Glossary}{
  function: a closure or primitive function.  A symbol or
  string refering to such a function may also be used.

  predicate: A function that returns a logical value
  (unary, unless stated otherwise).  A symbol or string
  refering to such a function may also be used.

  collection: A list, pairlist or vector of any type.

  arity: The number of arguments a function can accept.
  Variadic functions have an arity of +Infinity,

  number: a double, integer or complex number, unless
  stated otherwise.
}
\examples{
inst/examples/blank.R
}

