
#' xPredOf
#'
#' Increment a vector of numbers.
#'
#' @param
#'    nums a vector of non-complex numbers.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of non-complex numbers.
#'
#' @section Corner Cases:
#'    Returns the unit of the vector if \code{nums} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPredOf.R
#'
#' @rdname xPredOf
#' @export

xPredOf <- function (nums) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of nums.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	insist $ must_be_collection(nums, invoking_call)

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		nums - 1
	}
}

#' @rdname xPredOf
#' @export

xPredOf... <- function (...) {
	xPredOf(list(...))
}
