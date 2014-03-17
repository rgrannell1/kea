
#' xTabulate
#'
#' Tabulate a collection into pairs of value:frequency lists.
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
	# Collection any -> Collection any
	# tabulate a collection into tuples of value: frequency

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {

		set <- unique(coll)

		indices <- vapply(coll, function (elem) {

			for (ith in seq_along(set)) {
				if (identical( elem, set[[ith]] )) {
					return(ith)
				}
			}

		}, numeric(1))

		index_frequencies <- as.list(table(indices))

		lapply(names(index_frequencies), function (ith) {

			ith <- as.integer(ith)

			list(set[[ith]], index_frequencies[[ith]])
		})
	}
})

#' @rdname xTabulate
#' @export

xTabulate... <- function (...) {
	xTabulate(list(...))
}
