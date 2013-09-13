
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
#' @examples 
#' @export

xForall <- function (pred, ...) {
	# does there not exist any choice of bindings for 
	# pred such that pred is false?

	pcall <- sys.call()

	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)
	
	pred <- match.fun(pred)
	colls <- list(...)

	assert(
		all( sapply(colls, function (coll) {
			is.vector(coll) || is.pairlist(coll) 
		}) ), pcall)

	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		True
	} else {

		assert (
			xArity(pred) %in% c(length(colls), Inf), pcall)
		
		modulo_iths <- function (n, mods) {
			
			assert(n <= prod(mods), pcall)
			as.numeric(arrayInd(n, .dim = mods))
		}

		ith <- 1
		while (ith <= prod(coll_lengths)) {

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
			} else {
				ith <- ith + 1
			}
		}
		True
	}
}
