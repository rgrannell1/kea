
#' Flip the sign of each number in a vector.
#'
#' xNegate
#'
#' @param nums a vector of numbers.
#'
#' @return a vector of number.
#'
#' @section Corner Cases:
#'     If nums is empty then the unit of the collection is returned.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xNegate <- function (nums) {
	# Collection number -> number
	# flips the sign of each number.

	parent_call <- sys.call()

	assert(
		!missing(nums), parent_call,
		exclaim$parameter_missing(nums))

	nums <- dearrowise(nums)

	assert(
		is_collection(nums), parent_call,
		exclaim$must_be_collection(nums))

	nums <- coerce_to_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		-nums
	}
}

#' @export

xNegate... <- function (...) {
	xNegate(list(...))
}
