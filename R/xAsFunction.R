
#' xAsFunction
#' 
#' Convert a collection to a function that takes an index.
#'
#' @param coll a collection
#'
#' @return a function that takes one or more indices.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xAsFunction <- function (coll) {
	# Collection any -> (... -> [any])
	# enclose a collection in a function, and
	# allow access by supplying indices.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	function (...) {

		nums <- c(...)

		assert(
			is.numeric(nums), pcall,
			exclaim$must_be_numeric(nums))

		assert(
			all(round(nums) == nums), pcall,
			exclaim$must_be_whole(nums))

		assert(
			length(coll) >= max(nums), pcall,
			exclaim$must_be_grequal_than("length(coll)", max(nums)))

		assert(
			min(nums) >= 0, pcall,
			exclaim$must_be_greater_than("min(nums)", 0))

		as.list(coll[nums])
	}
}
