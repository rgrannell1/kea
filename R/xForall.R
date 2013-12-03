
#' xForall
#'
#' Does ever selection of arguments satisfy a predicate?
#'
#' @param pred an n-ary predicate.
#' @param colls n collections.
#'
#' @return a boolean value.
#'
#' @section Corner Cases:
#'     if any collection is length zero (or no collections are given), then False is returned.
#'
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xForall <- function (pred, colls) {
	# does there not exist any choice of bindings for
	# pred such that pred is false?

	parent_call <- sys.call()

	assert(
		!missing(pred), parent_call,
		exclaim$parameter_missing(pred))

	fn <- dearrowise(pred)

	assert(
		is_fn_matchable(pred), parent_call,
		exclaim$must_be_matchable(pred))

	pred <- match.fun(pred)
	colls <- lapply(colls, dearrowise)

	assert(
		all( sapply(colls, function (coll) {
			is_collection(coll)
		}) ), parent_call)

	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		True
	} else {

		modulo_iths <- function (n, mods) {

			assert(n <= prod(mods), parent_call)
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
				parent_call)

			assert(is.logical(is_match), parent_call)

			if (!isTRUE(is_match)) {
				return (False)
			}
		}
		True
	}
}

#' @export

xForall... <- function (fn, ...) {
	xForall(fn, list(...))
}
