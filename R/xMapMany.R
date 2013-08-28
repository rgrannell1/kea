
#' xMapMany
#' 
#' Map an n-ary function over n-collections.
#'
#' @param fn
#' @param ... n lists, pairlists or vectors.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	if the shortest collection is length-zero, the empty list is returned.
#'	if the lengths of the the collections are not equal, then elements are recycled in 
#'	the shorter vectors
#'
#' @family 
#' @aliases
#'
#' @examples 
#' @export

#| function: xMapMany version: 0.1 finished: false

xMapMany <- function (fn, ...) {
	# function -> Collection any .... -> [any]

	pcall <- sys.call()
	require_a("functionable", fn, pcall)

	fn <- match.fun(fn)
	colls <- list(...)

	coll_lens <- sapply(colls, length)

	if (length(colls) == 0 || 0 %in% coll_lens) {
		list()
	} else {
		max_length <- max(coll_lens)

		lapply(
			seq_len(max_length),
			function (ind) {

				tuple <- lapply(
					colls,
					function (elem) {
						this_ind <- ( (ind-1) %% (length(elem)) )+1
						elem[[this_ind]]
					}
				)
				do.call(fn, tuple)
		})
	}
}
