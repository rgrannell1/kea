
#' xNegate
#'
#' Flip the sign of each number in a vector.
#'
#' @param
#'    nums a vector of numbers.
#'
#' @return
#'    a vector of number.
#'
#' @section Corner Cases:
#'    If nums is empty then the unit of the collection is returned.
#'
#' @export

xNegate <- function (nums) {
	# Collection number -> number
	# flips the sign of each number.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parameter_missing(nums))



	assert(
		is_collection(nums), invoking_call,
		exclaim$must_be_collection(nums))

	nums <- as_typed_vector(nums, 'numeric')

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
