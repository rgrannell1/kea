
#' xExplode
#'
#' Split a string into a character vector using a regular expression.
#'
#' @param
#'    rexp a regular expression.
#'
#' @param
#'    str a string.
#'
#' @return
#'    A character vector.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll} is length-zero.
#'    If no match is found the original string is returned.
#'
#' @family character_functions
#'
#' @rdname xExplode
#' @export

xExplode <- function (rexp, str) {
	# Vector string -> Vector string -> Vector str
	# split a str into substrs at a rexp.

	invoking_call <- sys.call()

	assert(
		!missing(rexp), invoking_call,
		exclaim$parametre_missing(rexp))

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	str <- as_typed_vector(str, "character", False)
	rexp <- as_typed_vector(rexp, "character", True)

	assert(
		maybe_atomic(rexp),
		exclaim$must_have_length(
			rexp, 0:1, summate(rexp)) )

	if (length(str) == 0) {
		character(0)
	} else if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, rexp)[[1]]
	}
}

#' @rdname xExplode
#' @export

xExplode... <- function (rexp, ...) {
	xExplode(rexp, list(...))
}
