
#' xIdentity
#'
#' Return an argument without modification.
#'
#' @section Uses:
#'    The identity function is useful for testing identities
#'    (for example, that mapping identity over a collection
#'	  should return the original collection).
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

	val <- dearrowise(val)

	val
}
#' @export

xIdiotBird <- xIdentity

#' @export

xI <- xIdiotBird
