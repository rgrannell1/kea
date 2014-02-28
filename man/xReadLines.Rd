\name{xReadChars}
\alias{xReadChars}
\alias{xReadLines}
\title{xReadLines}
\usage{
xReadChars(str)

xReadLines(str)
}
\arguments{
  \item{str}{a length-one character vector. The path to
  import files from.}

  \item{str}{a length-one character vector. The path to
  import files from.}
}
\value{
A character vector, with one or more elements.

A character vector, with one or more elements.
}
\description{
Import each line in a file to an element in a character
vector.

Import each line in a file to an element in a character
vector.
}
\section{Corner Cases}{
  xReadChars ignores missing terminal newlines.
}

\section{Corner Cases}{
  xReadLines ignores missing terminal newlines.
}
\examples{

NULL

NULL
}
\seealso{
Other character_functions: \code{\link{xExplode}},
\code{\link{xExplode...}}; \code{\link{xFromChars}},
\code{\link{xFromChars...}}; \code{\link{xFromLines}},
\code{\link{xFromLines...}}; \code{\link{xFromWords}},
\code{\link{xFromWords...}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xReadWords}};
\code{\link{xSubstring}}, \code{\link{xSubstring...}};
\code{\link{xToChars}}; \code{\link{xToLines}};
\code{\link{xToWords}}; \code{\link{xWriteChars}};
\code{\link{xWriteLines}}; \code{\link{xWriteWords}}

Other character_functions: \code{\link{xExplode}},
\code{\link{xExplode...}}; \code{\link{xFromChars}},
\code{\link{xFromChars...}}; \code{\link{xFromLines}},
\code{\link{xFromLines...}}; \code{\link{xFromWords}},
\code{\link{xFromWords...}}; \code{\link{xImplode}},
\code{\link{xImplode...}}; \code{\link{xReadWords}};
\code{\link{xSubstring}}, \code{\link{xSubstring...}};
\code{\link{xToChars}}; \code{\link{xToLines}};
\code{\link{xToWords}}; \code{\link{xWriteChars}};
\code{\link{xWriteLines}}; \code{\link{xWriteWords}}

Other inpure_functions: \code{\link{xDelay}};
\code{\link{xDo}}, \code{\link{xDo...}};
\code{\link{xExecute}}; \code{\link{xReadWords}};
\code{\link{xStopwatch}}; \code{\link{xVersion}};
\code{\link{xWriteChars}}; \code{\link{xWriteLines}};
\code{\link{xWriteWords}}

Other inpure_functions: \code{\link{xDelay}};
\code{\link{xDo}}, \code{\link{xDo...}};
\code{\link{xExecute}}; \code{\link{xReadWords}};
\code{\link{xStopwatch}}; \code{\link{xVersion}};
\code{\link{xWriteChars}}; \code{\link{xWriteLines}};
\code{\link{xWriteWords}}
}

