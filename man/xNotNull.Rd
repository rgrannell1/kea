% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/xNotNull.R
\name{xNotNull}
\alias{xNotNull}
\title{xNotNull}
\usage{
xNotNull(val)
}
\arguments{
\item{val}{an arbitrary value. The value to test for
   not being null.}

\item{...}{see above.}
}
\value{
A true or false value.
}
\description{
Is an value not null?
}
\section{Type Signature}{

    any -> <logical>
}

\section{Corner Cases}{

    xNotNull returns either true or false, to make it
    safe for use with if statements.
}
\examples{

# 1. An obvious example.

xNotNull(Null)

# False

# 2. filter out null values.

x__(1, 2, NULL, 3) $ xSelect(xNotNull)

# list(1, 2, 3)
}
\seealso{
Other value_testing_functions: \code{\link{xIsEmpty}},
  \code{\link{xIsEmpty_}}; \code{\link{xIsFalse}};
  \code{\link{xIsInfixOf}}, \code{\link{xIsInfixOf_}};
  \code{\link{xIsNan}}; \code{\link{xIsNa}};
  \code{\link{xIsNull}}; \code{\link{xIsPrefixOf}},
  \code{\link{xIsPrefixOf_}}; \code{\link{xIsSuffixOf}},
  \code{\link{xIsSuffixOf_}}; \code{\link{xIsTrue}};
  \code{\link{xIs}}; \code{\link{xNotEmpty}},
  \code{\link{xNotEmpty_}}; \code{\link{xNotFalse}};
  \code{\link{xNotInfixOf}}, \code{\link{xNotInfixOf_}};
  \code{\link{xNotNan}}; \code{\link{xNotNa}};
  \code{\link{xNotPrefixOf}}, \code{\link{xNotPrefixOf_}};
  \code{\link{xNotSuffixOf}}, \code{\link{xNotSuffixOf_}};
  \code{\link{xNotTrue}}; \code{\link{xNot}}
}

