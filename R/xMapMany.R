
#' xMapMany
#' 
#' Map an n-ary function over n-collections/
#'
#' @param fn a n-ary function.
#' @param ... n collections.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if any collection is length-zero. If the 
#'     collection lengths are not equal then elements are recycles in the shorter collections.
#' @template glossary
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
