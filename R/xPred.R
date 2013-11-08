
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
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xPred <- function (nums) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of nums.

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
		nums - 1
	}
}

#' @export

xPred... <- function (...) {
	xPred(list(...))
}
