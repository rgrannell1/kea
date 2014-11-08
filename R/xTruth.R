
#' xTruth
#'
#' A function that always returns true.
#'
#' @section Type Signature:
#'     ...any -> <logical>
#'
#' @param
#'    ... arguments to be dropped.
#'
#' @return
#'    true
#'
#' @section Corner Cases:
#'    Arguments given to \bold{xTruth} are dropped.
#'
#' @family basic_functions
#'
#' @example
#'    inst/examples/example-xTruth.R
#'
#' @rdname xTruth
#' @export

xTruth <- MakeFun(function (...) {TRUE})
