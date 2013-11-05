
#' xMapMany
#'
#' Map a function across many collections simultaneously.
#'
#' @param fn a n-ary function.
#' @param ... n collections.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if any collection is length-zero. If the
#'     collection lengths are not equal then elements are recycled in the shorter collections.
#' @template glossary
#'
#'
#' @family higher_order_functions map_like_functions
#'
#' @example inst/examples/blank.R
#' @export

xMapMany <- function (fn, ...) {
	# function -> Collection any .... -> [any]
	# Map a function across many collections simultaneously.

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	colls <- lapply(list(...), dearrowise)

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
					function (coll) {
						this_ind <- ( (ind-1) %% (length(coll)) )+1
						coll[[this_ind]]
					}
				)
				do.call(fn, tuple)
		})
	}
}
