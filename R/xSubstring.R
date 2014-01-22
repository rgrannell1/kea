
#' xSubstring
#'
#' Subset a string using normal R vector indexing.
#'
#' @param
#'    str a string.
#'
#' @param
#'    nums indices of \code{str}.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A character vector.
#'
#' @section Corner Cases:
#'      Returns the empty list if \code{coll} is length-zero.
#'
#' @family character_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xSubstring
#' @export

xSubstring <- function (str, nums) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	str <- as_typed_vector(str, "character", True)
	nums <- as_typed_vector(nums, "numeric", False)

	insist$must_be_of_length(str, 0:1, invoking_call)
	insist$must_be_whole(nums, invoking_call)

	if (length(str) == 0 || (length(nums) == 1 && nums == 0)) {
		character(0)
	} else if (length(nums) == 0) {
		str
	} else {
		assert(
			max(nums) <= nchar(str), invoking_call)

		chars <- strsplit(str, "")[[1]]
		paste0(chars[nums], collapse = "")
	}
}

#' @rdname xSubstring
#' @export

xSubstring... <- function (str, ...) {
	xSubstring(str, list(...))
}
