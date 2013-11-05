
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

	str <- match.call()$str

	assert(
		(is.character(str) && length(str) == 1) ||
		is.name(str), parent_call,
		exclaim$must_be_string(str))

	exists(toString(str), pframe) &&
		bindingIsLocked(str, pframe)
}
