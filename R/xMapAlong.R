
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

#| function: xMapAlong version: 0.1 finished: false 

xMapAlong <- function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]

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