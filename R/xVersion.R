
#' xVersion
#'
#' Get the current version number of Arrow.
#'
#' @section Type Signature:
#'     ...any -> &lt;integer>
#'
#' @details
#'     Arrow versioning follows the semantic version
#'     standard (2.0.0), which is summarised by the author as
#'
#'  "Given a version number MAJOR.MINOR.PATCH, increment the:
#'
#'  MAJOR version when you make incompatible API changes,
#'  MINOR version when you add functionality in a backwards-compatible manner, and
#'  PATCH version when you make backwards-compatible bug fixes."
#'
#'  For pre 1.0.0 versions Arrow's minor version is bumped anytime a backwards-
#'  incompatable change is made, or new functionality is added.
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
	c(0L, 8L, 0L)
}
