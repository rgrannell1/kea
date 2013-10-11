
#' xForall
#' 
#' Does ever selection of arguments satisfy a predicate?
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

xForall <- function (pred, ...) {
	# does there not exist any choice of bindings for 
	# pred such that pred is false?

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
		all( sapply(colls, function (coll) {
			is_collection(coll) 
		}) ), pcall)

	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		True
	} else {
		
		modulo_iths <- function (n, mods) {
			
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
			
			if (!is_match) {
				return (False)
			}
		}
		True
	}
}
