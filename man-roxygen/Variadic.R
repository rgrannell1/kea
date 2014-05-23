
#' @section Variadic Functions:
#'
#' Arrow functions often have two forms; xFoo and the underscored xFoo_. These
#' two forms differ in how they collect their arguments; the function xFoo will
#' take a list or vector of arguments, while the function xFoo_ will take its
#' arguments through the ellipsis argument.
#'
#' The function \bold{xMap} - as suggested by the above - takes a collection as its
#' second argument.
#'
#' \code{xMap(toupper), list('a', 'b', 'c', 'd')}
#'
#' \bold{xMap_} takes an unlimited number of arguments. These arguments are collected by
#' \bold{xMap_}, so the user does not need to wrap them in a list first.
#'
#' \code{xMap_(toupper, 'a', 'b', 'c', 'd')}
#'
#' Both forms are useful. The variadic form, xFoo_, is most useful when writing
#' code with a fixed number of inputs:
#'
#' xFix_(xMap, sqrt)
#'
#' This is nicer than writing
#'
#' xFix(xMap, list(sqrt))
#'
#' The default form, xFoo, is more useful when you are using variable data instead of
#' data literals.
#'
#' Both variadic and non-variadic forms of functions are implemented to cut down
#' on boilerplate. In base R you will often seem code of the form
#'
#' colls <- list(list(1, 2), list(3, 4))
#' \code{do.call(cbind, colls)}
#'
#' Or (less generally)
#'
#' \code{Reduce(cbind, colls)}
#'
#' The use of \bold{do.call} or \bold{Reduce} to adapt variadic functions (xFoo_)
#' to work with variable-length data is not required by arrow, since both forms
#' are usually available.
#'
#' xJoin_(list(1, 2), list(3, 4))
#' xJoin(colls)
#'
