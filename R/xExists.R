
#' xExists
#'
#' Does any selection of arguments satisfy a predicate?
#'
#' @param
#'    pred an n-ary predicate.
#'
#' @param
#'    colls n collections.
#'
#' @return
#'    a boolean value.
#'
#' @section Corner Cases:
#'    if any collection is length zero (or no collections are given),
#'    then False is returned.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    collection_functions
#'
#' @export

xExists <- function (pred, colls) {
	# does there exist any choice of bindings for
	# pred such that pred is true?

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parameter_missing(pred))

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(pred))

	pred <- match.fun(pred)

	assert(
		all(sapply(colls, is_collection)), invoking_call,
		exclaim$must_be_recursive_of_collections(colls))

	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		False
	} else {

		modulo_iths <- function (n, mods) {
			# get the nth expanded index of an oddly-shaped array.

			assert(n <= prod(mods), invoking_call)
			as.numeric(arrayInd(n, .dim = mods))
		}

		for ( ith in seq_len(prod(coll_lengths)) ) {

			indices <- modulo_iths(ith, coll_lengths)

			tuple <- Map(
				function (coll_ith) {
					choice <- indices[coll_ith]
					colls[[coll_ith]][[choice]]
				},
				seq_along(colls)
			)

			is_match <- try_higher_order(
				do.call(pred, tuple),
				invoking_call)

			assert(
				is.logical(is_match), invoking_call)

			if (isTRUE(is_match)) {
				return (True)
			}
		}
		False
	}
}

#' @export

xExists... <- function (pred, ...) {
	xExists(pred, list(...))
}
