
#' Appendix A
#'
#' Naming Conventions
#'
#' Arrow tries to employ consistent naming conventions to
#' minimise the verbosity of code. The conventions are fairly simple,
#' and only a handful need to be committed to memory.
#'
#' @section I Function Names:
#'
#' All arrow functions are prefixed with the letter 'x', followed by an
#' uppercase letter. Arrow function names are camel-case, as R's base
#' higher-order functions follow this convention.
#'
#' \code{xMap}
#'
#' \code{xStopwatch}
#'
#' \code{xDeepMap}
#'
#' Both English and American English spellings are accepted; for example
#' \code{xVectorise} has an americanised counterpart.
#'
#' Where possible, functions come in pairs with an similar function.
#' For example, \code{xSelect} has a counterpart \code{xReject}, and
#' \code{xImplode} has a counterpart \code{xExplode}.
#'
#' Many arrow functions are variadic (they call the '...' argument). Where
#' possible, every function have a variadic and non-variadic form.
#' As an example, \code{xSelect} takes an argument \code{coll} that
#' is a collection, while the function \code{xSelect...} takes that same
#' collection from the arguments passed to '...'.
#'
#' \code{xMap}
#'
#' \code{xMap...}
#'
#' \code{xStopwatch}
#'
#' \code{xDeepMap}
#'
#' \code{xDeepMap...}
#'
#' Certain fold and search function have 'left' and 'right' forms, which
#' dictates the order of the operation carried out. This is encoded by
#' the letter 'l' or 'r', appended to the base function name. The left
#' form of these operations can also be called without an additional 'l'.
#'
#' \code{xFold}
#' \code{xFoldl}
#' \code{xFoldr}
#'
#' A minority of functions have infix operator forms, or single letter
#' shorthands.
#'
#' @section II Parametre Names:
#'
#' Arrow only uses a handful of parametre names, each of which captures
#' some information about the expected input:
#'
#' \itemize{
#'      \item{\bold{bool:}}}{ a single boolean value. }
#'      \item{\bold{bools:}}}{ a collection of boolean values. }
#'      \item{\bold{coll:}}{ a collection. }
#'      \item{\bold{comps:}}}{ a collection of complex values. }
#'      \item{\bold{colls:}}{ a collection of collections. }
#'      \item{\bold{fn:}}{ a collection of collections. }
#'      \item{\bold{num:}}{ a single number. }
#'      \item{\bold{nums:}}{ a collection of numbers. }
#'      \item{\bold{pred:}}{ a function that returns a logical value. }
#'      \item{\bold{rexp:}}{ a string to be used as a regular expression. }
#'      \item{\bold{str:}}{ a string. }
#'      \item{\bold{strs:}}{ a collection of strings. }
#'      \item{\bold{sym:}}{ a symbol or string. }
#'      \item{\bold{val:}}{ any R value. }
#' }
#'
#' All functions should be assummed to be unary, unless otherwise stated.
#'
#' If multiple parametres of the same rough properties are needed then a
#' number is appended to the end of the parametre name; for example
#' \code{fn1, fn2, ...}.
#'
#' @section III Method Names:
#'
#' The method naming conventions are slightly more elaborate that that
#' for normal functions. Every method has a chaining and unchaining version,
#' with the chaining version returning an arrow object, and the unchaining
#' version returning only the contents of that object.
#'
#' The unchaining methods are of the form \code{x_Method} or \code{x_Method...},
#' while the chaining methods have the form  \code{xMethod} or \code{xMethod...},
#' Otherwise, the normal function naming conventions apply.
#'
#' @name help_arrow_naming

NULL
