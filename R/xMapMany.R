
#' xMapMany
#'
#' Map a function across many collections simultaneously.
#'
#' @param
#'    fn a n-ary function.
#'
#' @param
#'    colls n collections.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if any collection is length-zero. If the
#'    collection lengths are not equal then elements are recycled in the shorter collections.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xMapMany
#' @export

xMapMany <- function (fn, colls) {
	# function -> Collection any .... -> [any]
	# Map a function across many collections simultaneously.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		all( vapply(colls, is_collection, logical(1)) ), invoking_call,
		exclaim$must_be_recursive_of_collections(
			colls, summate(colls)) )

	fn <- match_fn(fn)

	coll_lens <- vapply(colls, length, integer(1))

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

#' @rdname xMapMany
#' @export

xMapMany... <- function (fn, ...) {
	xMapMany(fn, list(...))
}
