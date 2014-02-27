\name{xTap}
\alias{xTap}
\title{$ xTap}
\usage{
x_(  ) $ xTap(fn)
}
\value{
An arrow object.
}
\description{
Apply an anonymous function to the contents of an arrow
object.
}
\details{
\bold{xTap} applies anonymous methods to arrow objects.
This serves the same purpose as anonymous functions in the
base language.

\code{x_(letters) $ xTap(xs := length(xs) == 26)}

\bold{xTap} also allows useful base functions to be used by
arrow objects.
}
\seealso{
Other method_functions: \code{\link{xByColnames}};
\code{\link{xByCols}}; \code{\link{xByLevels}},
\code{\link{xByLevels}}; \code{\link{xByRows}};
\code{\link{xColUnit}}; \code{\link{xElemsByCols}};
\code{\link{xElemsByRows}}; \code{\link{xExecute}};
\code{\link{xRowUnit}}; \code{\link{xTranspose}};
\code{\link{x_}}
}

