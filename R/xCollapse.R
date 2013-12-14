
#' xCollapse
#'
#' Concatenate a character vector into a string with a delimiter.
#'
#' @param
#'    str a string to use as a delimiter.
#'
#' @param
#'    strs several character vectors.
#'
#' @return
#'    a length-one character vector.
#'
#' @family
#'    character_vector_functions
#'
#' @export

xCollapse <- function (str, strs) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parameter_missing(str))

	assert(
		is_collection(str), invoking_call,
		exclaim$must_be_collection(str))

	str <- as_typed_vector(
		str, 'character', True)

	assert(
		is_collection(strs), invoking_call,
		exclaim$must_be_collection(strs))

	strs <- as_typed_vector(
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
