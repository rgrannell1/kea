
#' xNegate
#'
#' Flip the sign of each number in a vector.
#'
#' @param
#'    nums a vector of numbers. The numbers to
#'    reverse the signs of.
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
#' @example
#'    inst/examples/example-xNegate.R
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

	insist $ must_be_collection(nums, invoking_call)

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
