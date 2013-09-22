
#' xMapAlong
#' 
#' Apply a binary function to each element of a collection. and its indices.
#'
#' @param fn a binary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list is \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @examples 
#' @export

xMapAlong <- function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]

	pcall <- sys.call()

	assert(
		!missing(fn), pcall)
	assert(
		!missing(coll), pcall)

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)
	
	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(2, Inf), pcall)

	if (length(coll) == 0) {
		list()
	} else {
		Map(
			function (ind) {
				fn( coll[[ind]], ind )
			},
			seq_along(coll)
		)
	}
}
