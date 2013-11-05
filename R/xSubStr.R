
#' xSubStr
#'
#' Subset a string using normal R vector indexing.
#'
#' @param str a string.
#' @param nums indices of \code{str}.
#'
#' @return a character vector.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xSubStr <- function (str, nums) {
	# str -> integer -> Vector str
	# subset a str using normal R vector indexing.

	parent_call <- sys.call()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	assert(
		!missing(nums), parent_call,
		exclaim$parameter_missing(nums))

	str <- dearrowise(str)
	nums <- dearrowise(nums)

	str <- coerce_to_typed_vector(str, "character", True)
	nums <- coerce_to_typed_vector(nums, "numeric", False)

	assert(
		is.character(str) && length(str) < 2, parent_call)

	assert(
		all(round(nums) == nums), parent_call)

	if (length(str) == 0) {
		character(0)
	} else if (length(nums) == 0) {
		str
	} else {
		assert(
			max(nums) <= nchar(str), parent_call)

		chars <- strsplit(str, "")[[1]]
		paste0(chars[nums], collapse = "")
	}
}
