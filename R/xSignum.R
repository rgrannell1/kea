
#' xSignum
#'
#' Get the sign of a vector of numbers.
#'
#' @param nums a vector of non-complex numbers.
#'
#' @return a vector of elements in the set \code{-1, 0, +1}.
#'
#' @section Corner Cases:
#'	 If \code{nums} is lenth-zero then the unit of that vector is returned.
#'	 The sign of zero is zero.
#'
#' @family variadic_functions
#'
#' @export

xSignum <- function (nums) {
	# Collection number -> Vector number
	# returns the sign of a number.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	assert(
		is_collection(nums), invoking_call,
		exclaim$must_be_collection(
			nums, profile_object(nums)) )

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		get_sign <- function (num) {
			if (num > 0) +1 else if (num == 0) 0 else -1
		}

		sapply(nums, get_sign)
	}
}

#' @export

xSignum... <- function (...) {
	xSignum(list(...))
}