
#' xIrrelevance
#'
#' @section Type Signature:
#'     ..any -> <logical>
#'
#' A function that always returns na.
#'
#' @param
#'    ... arguments to be dropped.
#'
#' @return
#'    na
#'
#' @section Corner Cases:
#'    None.
#'
#' @family basic_functions
#'
#' @example
#'    inst/examples/example-xIrrelevance.R
#'
#' @rdname xIrrelevance
#' @export
#
# function name suggested by 'bib' from
# english.stackexchange.

xIrrelevance <- MakeFun(function (...) {NA})
