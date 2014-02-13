
#' xIsVal
#'
#' Is a name binding locked?
#'
#' @param
#'    sym a symbol or string. The variable name
#'    to test.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If a variable isn't assigned, then \code{false} is returned.
#'
#'
#' @example
#'    inst/examples/example-xIsVal.R
#'
#' @rdname xIsVal
#' @export

xIsVal <- function (str) {
	# symbol | Vector character -> logical
	# is a name binding locked?

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	insist $ must_not_be_missing(str)

	str <- toString(match.call()$str)
	insist $ must_be_collection(str, invoking_call)

	str <- unit_to_value(as_typed_vector(str, "character"))

	insist $ must_be_of_length(str, 1, invoking_call)

	exists(str, parent_frame) &&
		bindingIsLocked(str, parent_frame)
}
