
#' xSignum
#'
#' Get the sign of a vector of numbers.
#'
#' @param
#'      nums a collection of numbers. The numbers to take
#'      the sign of.
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
#' @example
#'    inst/examples/example-xSignum.R
#'
#' @rdname xSignum
#' @export

xSignum <- function (nums) {
	# Collection number -> integers
	# returns the sign of a number vector.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	insist$must_be_collection(nums, invoking_call)

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
