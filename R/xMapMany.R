
#' xMapMany
#'
#' Map a function across many collections simultaneously.
#'
#' @param fn a n-ary function.
#' @param colls n collections.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if any collection is length-zero. If the
#'     collection lengths are not equal then elements are recycled in the shorter collections.
#'
#'
#'
#' @family higher_order_functions map_like_functions collection_functions
#'

#' @export

xMapMany <- function (fn, colls) {
	# function -> Collection any .... -> [any]
	# Map a function across many collections simultaneously.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	colls <- lapply(colls, dearrowise)

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
				try_higher_order(
					do.call(fn, tuple),
					invoking_call)
		})
	}
}

#' @export

xMapMany... <- function (fn, ...) {
	xMapMany(fn, list(...))
}
