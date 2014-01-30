
#' xOrder
#'
#' Return a permutation of indices giving the order of an integer vector.
#'
#' @param
#'    nums a vector of numbers. The numbers to rank
#'    in order of size.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of numbers.
#'
#' @section Corner Cases:
#'    If nums is empty then \bold{integer(0)} is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xOrder.R
#'
#' @rdname xOrder
#' @export

xOrder <- function (nums) {
	# Collection numbers -> Vector numbers
	# Return a permutation giving the order
	# of nums.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	insist $ must_be_collection(nums, invoking_call)

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		integer(0)
	} else {
		as.integer(order(x = nums))
	}
}

#' @rdname xOrder
#' @export

xOrder... <- function (...) {
	xOrder(list(...))
}
