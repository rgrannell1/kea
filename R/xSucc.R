
#' xSucc
#'
#' Decrement a collection of numbers.
#'
#' @param
#'    nums a collection of numbers.
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
#' @rdname xSucc
#' @export

xSucc <- function (nums) {
	# Collection number -> Vector number
	# returns the successor of a vector of nums.

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
		nums + 1
	}
}

#' @rdname xSucc
#' @export

xSucc... <- function (...) {
	xSucc(list(...))
}