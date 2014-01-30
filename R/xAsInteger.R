
#' xAsInteger
#'
#' Convert a collection to a integer vector.
#'
#' @details
#'    \code{xAsInteger} converts a list, pairlist or vector of
#'    length-one integers to a integer vector. It does not attempt
#'    to convert non-integer collections to integer vectors.
#'
#' @param
#'    nums a collection of integer values. A list, pairlist or vector
#'    of length-one integer vectors to convert to a integer vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An integer vector.
#'
#' @section Corner Cases:
#'    Double vectors are not coerced to integer vectors:
#'    doubles can be decimal or infinite numbers, which
#'    would be lost upon coersion.
#'
#' @template
#'    Variadic
#'
#'
#' @example
#'    inst/examples/example-xAsInteger.R
#'
#' @rdname xAsInteger
#' @export

xAsInteger <- function (nums) {
	# Collection integer -> Vector integer
	# convert a collection to a integer vector.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	insist $ must_be_collection(nums, invoking_call)

	nums <- as_typed_vector(nums, 'numeric')

	insist $ must_be_whole(nums, invoking_call)

	if (length(nums) == 0) {
		integer(0)
	} else {
		as.integer(nums)
	}
}

#' @rdname xAsInteger
#' @export

xAsInteger... <- function (...) {
	xAsInteger(list(...))
}
