
#' xIsVal
#'
#' Is a name binding locked?
#'
#' @param
#'    str a string or symbol.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If a variable isn't assigned, then \code{false} is returned.
#'
#' @rdname xIsVal
#' @export

xIsVal <- function (str) {
	# symbol | Vector character -> logical
	# is a name binding locked?

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	str <- toString(match.call()$str)
	str <- as_typed_vector(str, "character", True)

	assert(
		length(str) == 1, invoking_call,
		exclaim$must_have_length(
			str, 1, summate(str)) )

	exists(str, parent_frame) &&
		bindingIsLocked(str, parent_frame)
}
