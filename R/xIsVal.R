
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
	insist$must_be_collection(str, invoking_call)

	str <- as_typed_vector(str, "character", True)

	insist$must_be_of_length(str, 1, invoking_call)

	exists(str, parent_frame) &&
		bindingIsLocked(str, parent_frame)
}
