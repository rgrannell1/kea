\name{help_arrow_gotchas}
\alias{help_arrow_gotchas}
\title{Usage - Arrow Gotchas}
\description{
Common gotchas in Arrow, and how to avoid them.
}
\section{Usage-Gotchas}{
  Arrow sometimes employs black magic to implement some of
  its more divergent features, which can occasionally
  present suprising behaviour.
}

\section{1. Arrow objects vs values}{
  Arrow's most common gotcha - particularily before
  unchaining methods were added - is the accidental misuse
  of chaining methods where unchaining methods are needed.

  \code{cumsum( x_(1:10)$xMap(sqrt) )}

  will throw an error; mean operates on numeric vectors,
  not arrow objects that contain numeric vectors. Valid
  alternatives are

  \code{cumsum( x_(1:10)$x_Map(sqrt) )}

  \code{cumsum( x_(1:10)$xMap(sqrt)$x_() )}

  or even

  \code{x_(1:10)$xMap(sqrt)$x_Tap(cumsum)}
}

\section{2. Arrow Functions}{
  Arrow functions are implemented using an arcane oversight
  in the R parser; the infix function ':=' can be parsed,
  but is never actually used by the base language. I
  co-opted this function to avoid having to name xLambda
  the horrible '\%=>\%'. Because the use of this function
  is somewhat of a hack there are some syntax-related
  gotchas.

  The function ':=' has really low operator preceedence,
  making it useful as an infix function creator. For this
  reason the following code returns the expected function.

  \code{f <- x := 2*x^2 + 15*x + 2}

  For certain expressions operator preceedence will not
  work as expected, causing a cryptic error. The following

  \code{ x := x^2 \%of\% x := x + 2 }

  Is actually interpreted as

  \code{ x := { x^2 \%of\% x := x + 2 } }

  rather than the composition of two functions. As is
  normally the case, parentheses can enforce the proper
  preceedence of the expression:

  \code{ (x := x^2) \%of\% (x := x + 2 ) }

  The other significant issues with syntax involve the
  formal arguments given to ':='. It is currently
  impossible to create a nullary function using ':=', since
  the expression '( )' actually throws an error in R. For
  this reason, the following will throw a parse error:

  \code{ () := print('hi') }

  A workaround will be found at a later date (time of
  writing: pre 0.1.0)

  Due to parser constraints, commas cannot be used to
  delimit arrow function formals.

  \code{ (a, b, c) := { a + b + c} }

  Throws an error, but the following is equivelant in
  intent and actually works:

  \code{ (a : b : c) := { a + b + c} }

  Since the former example throws a parse error, it should
  be easy enough to spot this particular error after
  reading this.
}

