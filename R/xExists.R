
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
#' @examples 
#' @export

xExists <- function (pred, ...) {
	# does there exist any choice of bindings for 
	# pred such that pred is true?

	pcall <- sys.call()
	require_a("functionable", pred, pcall)

	pred <- match.fun(pred)

	colls <- list(...)

	require_a("list_of_collection", colls, pcall)
	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		False
	} else {

		modulo_iths <- function (n, mods) {
			if (n > prod(mods)) {
				stop("out of bounds")
			} else {
				as.numeric(arrayInd(n, .dim = mods))
			}
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

			if (!is.logical(is_match)) {
				stop("non-logical value produced")
			}

			if (is_match) {
				return (True)
			} else {
				ith <- ith + 1
			}
		}
		False
	}
}
