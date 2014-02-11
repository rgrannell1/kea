\docType{data}
\name{format_call}
\alias{Truth}
\alias{assert}
\alias{colourise}
\alias{demand}
\alias{format_call}
\alias{insist}
\alias{write_error}
\title{Package Internals}
\format{\preformatted{List of 5
 $ black :function (message)  
 $ red   :function (message)  
 $ green :function (message)  
 $ blue  :function (message)  
 $ yellow:function (message)  
}}
\usage{
format_call(call)

colourise

write_error(..., call. = True)

assert(expr, invoking_call, message)

insist

demand

Truth(...)
}
\description{
Documentation for the internals of Arrow. Every
sufficiently large utility library will contain its own
utility library. These functions are required to reduce
repetition of code when implementing arrow functions.
}
\section{format_call}{
  format_call is a tool for formatting the source call of
  an error message. It fixes as issue with printing ':='
  functions, and cuts off very long calls.
}

\section{colourise}{
  colourise is a set of functions that wraps strings in
  ansii escape sequences on coloured terminals, allowing
  coloured text to be printed. These are used to colour
  error messages and print methods.
}

\section{write_error}{
  write_error prints a red error message to the terminal,
  if the terminal supports colour.
}

\section{assert}{
  assert is a key arrow function - it takes an expression,
  a call to display, and a string message. If the
  expression isn't true, the error is thrown with that
  message and call.
}

\section{insist}{
  insist is a list of functions that provide a minimal
  interface to an assertion. This object exists to reduce
  the amount of assertion checking code needed.

  \code{insist $ must_be_fn_matchable(fn, invoking_call)}

  Each function encloses a message function to stop the
  function being repeatedly created when assertions are
  ran. When the assertion inside the insist function fails,
  an error is thrown.
}

\section{demand}{
  An internal object containing assertions that are
  required by the collection comprehension functions.
  Mostly contains parse error assertions.
}
\keyword{internal}

