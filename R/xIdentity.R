
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
#' @example
#'    inst/examples/example-xIdentity.R
#'
#' @rdname xIdentity
#'
#' @export

xIdentity <- function (val) {

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	val
}

#' @rdname xIdentity
#' @export

xI <- xIdentity
