
#' xSignum
#'
#' Get the sign of a vector of numbers.
#'
#' @param
#'      nums a vector of non-complex numbers.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A integer vector of elements in the set \code{{-1, 0, +1}}.
#'
#' @section Corner Cases:
#'	 If \code{nums} is lenth-zero then the unit of that vector is returned.
#'	 The sign of zero is zero.
#'
#' @template
#'    Variadic
#'
#' @rdname xSignum
#' @export

xSignum <- function (nums) {
	# Collection number -> integers
	# returns the sign of a number.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	assert(
		is_collection(nums), invoking_call,
		exclaim$must_be_collection(
			nums, summate(nums)) )

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		get_sign <- function (num) {
			if (num > 0) +1L else if (num == 0) 0L else -1L
		}

		vapply(nums, get_sign, integer(1))
	}
}

#' @rdname xSignum
#' @export

xSignum... <- function (...) {
	xSignum(list(...))
}