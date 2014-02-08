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

Collection comprehensions are a shorthand for several
operations; taking the \bold{set product} of several
Collections, \bold{selecting} them based on a predicate,
and \bold{mapping} a function over each result.

\bold{1 The output expression}

The output expression corresponds to the map operation
mentioned above.  The for comprehension

\code{xList[2*x, x <- 1:10]}

is loosely translated into normal arrow code as

\code{xMapply(x := x^2, 1:10)}

\bold{2 Variable bindings}

At least one variable must be bound for a non-empty list
comprehension. Multiple variables may also be bound.

\code{xList[list(x, y), x <- 1:3, y <- 1:3]}

The above comprehension loosely translates as

\code{x_(xSetProd(1:3, 1:3)) $ xMapply((x : y) := c(x, y))}

\bold{3 The predicate expression (optional)}

If the final expression given to a comprehension is not a
binding expression it is treated as a predicate for
selecting particular values. This is optional.

\code{xList[c(a, b), a <- 1:3, b <- 1:3, a + b > 2]}

In the above pairs of a, b such that their sum is larger
than two are returned.
}

