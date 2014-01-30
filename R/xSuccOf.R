
#' xSuccOf
#'
#' Increment a collection of numbers.
#'
#' @param
#'    nums a collection of numbers. The numbers to increment.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of numbers.
#'
#' @section Corner Cases:
#'    If \code{nums} is empty then the unit of that vector is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSuccOf.R
#'
#' @rdname xSuccOf
#' @export

xSuccOf <- function (nums) {
	# Collection number -> Vector number
	# returns the successor of a vector of nums.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	insist $ must_be_collection(nums, invoking_call)

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		nums + 1
	}
}

#' @rdname xSuccOf
#' @export

xSuccOf... <- function (...) {
	xSuccOf(list(...))
}