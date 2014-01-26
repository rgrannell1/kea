
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
#'    Returns the empty list if \bold{coll} is length-zero.
#'    If no match is found the original string is returned.
#'    Every element of the returned character vector has one
#'    or more characters - no zero-character elements are ever
#'    generated.
#'
#' @family character_functions
#'
#'
#' @example
#'    inst/examples/example-xExplode.R
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

	str <- as_typed_vector(str, "character")
	rexp <- to_value_unit(as_typed_vector(rexp, "character"))

	insist$must_be_of_length(str, 0:1, invoking_call)

	if (length(str) == 0) {
		character(0)
	} else if (nchar(str) == 0) {
		''
	} else {
		exploded <- strsplit(str, rexp)[[1]]
		exploded[nchar(exploded) > 0]
	}
}

#' @rdname xExplode
#' @export

xExplode... <- function (rexp, ...) {
	xExplode(rexp, list(...))
}
