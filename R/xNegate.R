
#' xNegate
#'
#' Flip the sign of each number in a vector.
#'
#' @param
#'    nums a vector of numbers.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of number.
#'
#' @section Corner Cases:
#'    If nums is empty then the unit of the collection is returned.
#'
#' @template
#'    Variadic
#'
#' @rdname xNegate
#' @export

xNegate <- function (nums) {
	# Collection number -> number
	# flips the sign of each number.

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
		-nums
	}
}

#' @rdname xNegate
#' @export

xNegate... <- function (...) {
	xNegate(list(...))
}
