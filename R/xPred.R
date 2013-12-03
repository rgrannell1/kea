
#' xPred
#'
#' Increment a vector of numbers.
#'
#' @param nums a vector of non-complex numbers.
#'
#' @return a vector of non-complex numbers.
#'
#' @section Corner Cases:
#'     returns the unit of the vector if \code{nums} is length-zero.
#'
#'
#'
#' @example inst/examples/blank.R
#' @export

xPred <- function (nums) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of nums.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parameter_missing(nums))

	nums <- dearrowise(nums)

	assert(
		is_collection(nums), invoking_call,
		exclaim$must_be_collection(nums))

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		nums - 1
	}
}

#' @export

xPred... <- function (...) {
	xPred(list(...))
}
