
#' xIdentity
#'
#' Return an argument without modification.
#'
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    Returns \code{val}.
#'
#' @family basic_functions
#'
#' @example
#'    inst/examples/example-xIdentity.R
#'
#' @rdname xIdentity
#'
#' @export

xIdentity <- function (val) {

	invoking_call <- sys.call()

	insist $ must_not_be_missing(val)

	val
}

#' @rdname xIdentity
#' @export

xI <- xIdentity
