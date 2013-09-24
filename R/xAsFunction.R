
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
#' @examples inst/examples/blank.R
#' @export

xAsFunction <- function (coll) {
	# Collection any -> (... -> [any])
	# enclose a collection in a function, and
	# allow access by supplying indices.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	function (...) {

		nums <- c(...)

		assert(
			is.numeric(nums), pcall)
		assert(
			all(round(nums) == nums), pcall)
		assert(
			max(nums) <= length(coll), pcall)
		assert(
			min(nums) >= 0, pcall)

		as.list(coll[nums])
	}
}
