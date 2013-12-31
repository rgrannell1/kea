
#' xForall
#'
#' Does ever selection of arguments satisfy a predicate?
#'
#' @param
#'    pred an n-ary predicate.
#'
#' @param
#'    colls n collections.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a boolean value.
#'
#' @section Corner Cases:
#'    if any collection is length zero (or no
#'    collections are given), then False is returned.
#'
#' @family quantifier_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xForall
#' @export

xForall <- function (pred, colls) {
	# does there not exist any choice of bindings for
	# pred such that pred is false?

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parametre_missing(pred))

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(
			pred, summate(pred)) )

	pred <- match.fun(pred)

	assert(
		all(sapply(colls, is_collection)),
		invoking_call)

	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		logical(0)
	} else {

		modulo_iths <- function (n, mods) {

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
				is.logical(is_match), invoking_call,
				exclaim$non_logical_predicate(
					pred, summate(is_match)) )

			if (!isTRUE(is_match)) {
				return (False)
			}
		}
		True
	}
}

#' @rdname xForall
#' @export

xForall... <- function (pred, ...) {
	xForall(pred, list(...))
}
