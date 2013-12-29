
#' xAsFunction
#'
#' Convert a collection to a function that takes an index.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a function that takes one or more indices.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll is length-zero}.
#'
#' @family collection_functions
#'
#' @family function_modifying_functions
#'
#' @family variadic_functions
#'
#' @example
#'    inst/examples/example-xAsFunction.R
#'
#'
#' @rdname xAsFunction
#' @export

xAsFunction <- function (coll) {
	# Collection any -> (... -> [any])
	# enclose a collection in a function, and
	# allow access by supplying indices.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	function (nums) {

		nums <- as_typed_vector(nums, 'numeric', True)

		assert(
			is.numeric(nums), invoking_call,
			exclaim$must_be_numeric(
				nums, profile_object(nums)) )

		assert(
			all(round(nums) == nums), invoking_call,
			exclaim$must_be_whole(
				nums, profile_object(nums)) )

		assert(
			length(coll) >= max(nums), invoking_call,
			exclaim$must_be_grequal_than(
				"length(coll)", max(nums)) )

		assert(
			min(nums) >= 0, invoking_call,
			exclaim$must_be_greater_than("min(nums)", 0))

		as.list(coll[nums])
	}
}

#'
#' @rdname xAsFunction
#' @export

xAsFunction... <- function (...) {
	xAsFunction(list(...))
}
