\name{xList}
\alias{xList}
\title{xList[ ]}
\usage{
xList(...)
}
\description{
Generate a list using set-builder notation.
}
\details{
\bold{xList} allows for shorthand construction of lists
using a form of set-builder notation.  The notation
typically looks like

\code{S = {2.x | x ε 1..100, x^2 > 3}}

which reads "S is the set of numbers '2x' from the numbers
1...100, where x^2 is larger than three".  In general the
syntax is of the form

\code{S = {output expression, variable ε set, predicate
expression}}

Arrow's syntax is similar

\code{S <- xList[ 2*x, x <- 1:10 x^2 > 3 ]}

\code{S <- xList[output expression, variable <- set,
predicate expression]}

Notice that the variable 'x' is explicitly bound; at
runtime the expressions get transformed to functions of
'x'. Multiple parametres may be bound.

\code{xList[ c(a, b), a <- 1:4, b <- 1:4, a + b > 2 ]}

In the above example, every pair of numbers (a, b) such
that their sum is greater than two is returned.

The first expression - the output expression - must always
be included.  The predicate expression is optional; if it
is excluded the final expression should be a binding
expression.

\code{xList[ c(a, b), a <- 1:4, b <- 1:4]}
}

