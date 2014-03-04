\docType{data}
\name{stringify_call}
\alias{Object}
\alias{Truth}
\alias{\%!in\%}
\alias{\%+\%}
\alias{\%equals\%}
\alias{call_with_params}
\alias{colourise}
\alias{dictate}
\alias{get_call_components}
\alias{insist}
\alias{is_collection}
\alias{is_fn_matchable}
\alias{join_env}
\alias{one_of}
\alias{stringify_call}
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
stringify_call(call)

colourise

write_error(..., call. = True)

get_call_components(invoking_call)

insist

dictate

Truth(...)

one_of(coll)

a \%equals\% b

call_with_params(fnname, fn)

x \%+\% y

x \%!in\% y

Object()

join_env(x, y)

is_fn_matchable(val)

is_collection(val)
}
\description{
Documentation for the internals of Arrow. Every
sufficiently large utility library will contain its own
utility library. These functions are required to reduce
repetition of code when implementing arrow functions.
}
\section{stringify_call}{
  stringify_call is a tool for formatting the source call
  of an error message. It fixes as issue with printing ':='
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

\section{one_of}{
  Return one value from a collection.
}

\section{equals}{
  R's equal operator doesn't work on lists or strange
  values. equals is a better measure of identity.
}

\section{call_with_params}{
  Construct a call to a function 'fnname' with the
  parametres of a second function. Useful for higher order
  functions.
}

\section{+}{
  Concatenate two strings.
}

\section{in}{
  An infix function to test for the non-membership of an
  element in a set.
}

\section{Object}{
  Construct an empty environment.
}

\section{join_env}{
  Join two environments together into one environment. This
  allows for inheritance of environments without having to
  traverse multiple environments.
}

\section{is_fn_matchable}{
  Is a value a function, or possibly the name of a
  function.
}

\section{is_collection}{
  Is a value a generic or atomic vector or a pairlist.
}
\keyword{internal}

