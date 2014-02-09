
#' xOrder
#'
#' Return a permutation of indices that reorders an integer vector.
#'
#' @details
#'    \bold{xOrder} returns the indices for a collection that are required to
#'    re-order it. For example,
#'
#'    \code{c(3, 1, 2)[ xOrder(c(3,1,2)) ]}
#'
#'    re-arranges the collection as   
#'
#'    \code{c(1, 2, 3)}
#'
#'    This is a trivial use of \bold{xOrder}; a more common use is to
#'    re-order a collection of collections by the indices of one row or column.
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

		nums[is.nan(nums) || is.na(nums)] <- -Inf
		ordering <- vector('integer', length(nums))

		for (ith in seq_along(nums)) {

			max_index <- which.max( nums )
			ordering[ith] <- max_index
			nums[max_index] <- NaN
		}

		ordering
	}
}

#' @rdname xOrder
#' @export

xOrder... <- function (...) {
	xOrder(list(...))
}
