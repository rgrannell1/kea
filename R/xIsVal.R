
#' xIsVal
#'
#' Is a name binding locked?
#'
#' @param str a string or symbol.
#'
#' @return a boolean value.
#'
#' @section Corner Cases:
#'     If a variable isn't assigned, then \code{false} is returned.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xIsVal <- function (str) {
	# symbol | Vector character -> logical
	# is a name binding locked?

	parent_call <- sys.call()
	pframe <- parent.frame()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- toString(match.call()$str)
	str <- coerce_to_typed_vector(str, "character", True)

	assert(
		length(str) == 1, parent_call,
		exclaim$must_have_length(str, 1))

	exists(str, pframe) &&
		bindingIsLocked(str, pframe)
}
