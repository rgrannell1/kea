
#' xIdentity
#' 
#' Return an argument without modification.
#'
#' @param val an arbitrary value.
#'
#' @return returns \code{val}.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xIdentity <- function (val) {

	pcall <- sys.call()

	assert(
		!missing(val), pcall,
		exclaim$parameter_missing(val))

	val
}
#' @export

xIdiotBird <- xIdentity

#' @export

xI <- xIdiotBird
