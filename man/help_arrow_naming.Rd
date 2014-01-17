\name{help_arrow_naming}
\alias{help_arrow_naming}
\title{Usage - Naming Conventions}
\description{
A guide to naming conventions.
}
\details{
Good function naming conventions can make a library easier
to use. The fantastic plyr library uses a two-letter prefix
to denote the input and output types of each ply function,
which makes it trivial to find the correct variant of ply
function. Arrow uses a few different naming conventions,
though most don't need to be remembered.

\bold{Function Names}

Arrow functions are always prefixed with the letter 'x',
followed by an uppercase letter. Method names are camel
case.

\code{xMap}

\code{xFold}

\code{xSetProd}

Functions often come in pairs of related functions;

\code{xExplode}

\code{xImplode}

\code{xSelect}

\code{xReject}

Certain functions process collections in a certain
direction; as is convention in Haskell these have a 'l'
(left) or 'r' (right) suffix.

\code{xFoldl}

\code{xFoldr}

Most arrow functions have a normal version and a ellipsis
(...) version; the ellipsis form has a three-dot suffix.

\code{xMap}

\code{xMap...}

\bold{Parametre Names}

Arrow has a small number of parametre names, which refer to
some property of the expected argument.

\itemize{ \item{\bold{bool:}}{ a single boolean value. }
\item{\bold{bools:}}{ a collection of boolean values. }
\item{\bold{coll:}}{ a collection. } \item{\bold{comps:}}{
a collection of complex values. } \item{\bold{colls:}}{ a
collection of collections. } \item{\bold{fn:}}{ a
collection of collections. } \item{\bold{num:}}{ a single
number. } \item{\bold{nums:}}{ a collection of numbers. }
\item{\bold{pred:}}{ a function that returns a logical
value. } \item{\bold{rexp:}}{ a string to be used as a
regular expression. } \item{\bold{str:}}{ a string. }
\item{\bold{strs:}}{ a collection of strings. }
\item{\bold{sym:}}{ a symbol or string. }
\item{\bold{val:}}{ any R value. } }

Arrow functions rarely have more than two parametres.

\bold{Method Names}

R doesn't have a native implementation of chaining methods,
so a few naming conventions are needed to make them behave
as nicely as the methods in Python or JavaScript.

Chaining methods - methods whose return value itself has
methods - use the normal Arrow naming conventions.

\code{x_( letters ) \$ xMap...(toupper)}

Chaining methods don't play nicely with normal functions;
if you want to use the return value of a method in either a
base R function or an Arrow function you need to use an
unchaining method. Unchaining methods are prefixed with
'x_'.

\code{length( x_( letters ) \$ x_Map(toupper) )}

\code{length( x_( letters ) \$ x_Map...(toupper) )}
}

