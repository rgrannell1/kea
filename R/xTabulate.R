
#' xTabulate
#'
#' Tabulate a collection into pairs of value:frequency lists.
#'
#' @section Type Signature:
#'    |any| -> ||any, <number>||
#'
#' @details
#'     \bold{xTabulate} is superficially similar to the base
#'     function \bold{table}: given a collection that may or
#'     may not contain duplicates, it calculates the frequencies
#'     of each unique element.
#'
#'     \code{xTabulate(c('y', 'n', 'y', 'y', 'n'))}
#'
#'     \code{list(list("y", 3), list("n", 2))}
#'
#'     The result of the tabulation is unsorted for efficiency;
#'     if sorting is required \bold{xSortBy} can be used.
#'
#' @param
#'    coll a collection. The values to find the frequency of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    If \bold{coll} is length-zero the empty list is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xTabulate.R
#'
#' @family reshaping_functions
#'
#' @rdname xTabulate
#' @export

xTabulate <- MakeFun(function (coll) {

	MACRO( Fix(xTabulate, coll) )

	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {

		unique_elements <- unique(coll)

		indices <- vapply(coll, function (elem) {

			for (ith in seq_along(unique_elements)) {

				if (identical( elem, unique_elements[[ith]] )) {
					return(ith)
				}
			}

		}, numeric(1), USE.NAMES = False)

		index_frequencies <- as.list(table(indices))

		lapply(names(index_frequencies), function (ith) {

			# -- reparse the index from string. Dumb, but efficient.
			ith <- as.integer(ith)

			list(unique_elements[[ith]], as.numeric(index_frequencies[[ith]]) )
		})
	}
})

#' @rdname xTabulate
#' @export

xTabulate_ <- MakeVariadic(xTabulate, 'coll')
