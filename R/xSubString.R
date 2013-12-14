
#' xSubString
#'
#' Subset a string using normal R vector indexing.
#'
#' @param
#'    str a string.
#'
#' @param
#'    nums indices of \code{str}.
#'
#' @return a character vector.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @export

xSubString <- function (str, nums) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parameter_missing(str))

	assert(
		!missing(nums), invoking_call,
		exclaim$parameter_missing(nums))




	str <- as_typed_vector(str, "character", True)
	nums <- as_typed_vector(nums, "numeric", False)

	assert(
		length(str) < 2, invoking_call,
		exclaim$must_be_lequal_than(str, 2))

	assert(
		all(round(nums) == nums), invoking_call,
		exclaim$must_be_whole(nums))

	if (length(str) == 0) {
		character(0)
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

#' @export

xSubString... <- function (str, ...) {
	xSubString(str, list(...))
}
