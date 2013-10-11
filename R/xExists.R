
#' xExists
#' 
#' Does any selection of arguments satisfy a predicate?
#'
#' @param pred an n-ary predicate.
#' @param ... n collections.
#'
#' @return a boolean value.
#'
#' @section Corner Cases: 
#'     if any collection is length zero (or no collections are given), then False is returned.
#' @template glossary
#'
#' @family higher_order_function
#'
#' @examples inst/examples/blank.R
#' @export

xExists <- function (pred, ...) {
	# does there exist any choice of bindings for 
	# pred such that pred is true?

	pcall <- sys.call()

	assert(
		!missing(pred), pcall,
		exclaim$parameter_missing(pred))

	assert(
		is_fn_matchable(pred), pcall,
		exclaim$must_be_matchable(pred))

	pred <- match.fun(pred)
	colls <- list(...)

	assert(
		all(sapply(colls, is_collection)), pcall,
		exclaim$must_be_recursive_of_collections(colls))

	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		False
	} else {

		modulo_iths <- function (n, mods) {
			# get the nth expanded index of an oddly-shaped array.

			assert(n <= prod(mods), pcall)
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

			is_match <- do.call(pred, tuple)

			assert(is.logical(is_match), pcall)

			if (is_match) {
				return (True)
			}
		}
		False
	}
}
