
#' Apply a function to each element of a collection and its indices.
#'
#' @param fn a binary function that takes a value as its left parameter 
#'     and an index as its right parameter, or a
#'     symbol or name identifying such a function.
#' @param coll a pairlist, list, or vector.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @return a list containing \code{fn} applied to each elements of \code{coll}.
#' @export

#| function: xMapAlong version: 0.1 finished: false 

xMapAlong <- function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]"

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)
	
	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

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