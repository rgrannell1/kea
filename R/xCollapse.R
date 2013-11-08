
#' xCollapse
#'
#' Concatenate a character vector into a string with a delimiter.
#'
#' @param str a string to use as a delimiter.
#' @param strs a number of character vectors.
#'
#' @return a length-one character vector.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xCollapse <- function (str, strs) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	parent_call <- sys.call()

	strs <- lapply(strs, dearrowise)

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- dearrowise(str)

	assert(
		is_collection(str), parent_call,
		exclaim$must_be_collection(str))

	str <- coerce_to_typed_vector(
		str, 'character', True)

	assert(
		is_collection(strs), parent_call,
		exclaim$must_be_collection(strs))

	strs <- coerce_to_typed_vector(
		strs, 'character')

	assert(
		length(str) %in% 0:1,
		exclaim$must_have_length(str, 0:1) )

	if (length(strs) == 0) {
		character()
	} else {
		paste(
			xReject(
				function (x) {
					nchar(x) == 0 || length(x) == 0
				},
				strs),
			collapse = str)
	}
}

#' @export

xCollapse... <- function (str, ...) {
	xCollapse(str, list(...))
}
