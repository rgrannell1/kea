
#' xVersion
#'
#' Get the current version number of Arrow.
#'
#' @section Type Signature:
#'     ...any -> <integer>
#'
#' @param
#'    ... arguments to be dropped.
#'
#' @return
#'    A three-number vector, containing the
#'    major release number, minor release number and patch number.
#'
#' @example
#'    inst/examples/example-xVersion.R
#'
#' @family inpure_functions
#'
#' @rdname xVersion
#' @export

xVersion <- function (...) {
	c(0L, 1L, 0L)
}
