
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
#'     The result of the tabulation is unsorted for efficiencies sake;
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

xTabulate <- function (coll) {
	# Collection any -> Collection any
	# tabulate a collection into tuples of value: frequency

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {

		pairs <- list()

		for (elem in coll) {

			is_matched <- False

			for (jth in seq_along(pairs)) {

				if (identical( elem, pairs[[jth]][[1]] )) {

					pairs[[jth]][[2]] <- pairs[[jth]][[2]] + 1
					is_matched <- True
					break

				}
			}

			if (!is_matched) {
				pairs <- c(pairs, list(list(elem, 1)))
			}

		}
		pairs
	}
}

#' @rdname xTabulate
#' @export

xTabulate... <- function (...) {
	xTabulate(list(...))
}

dput(xTabulate( c('a', 'a', 'b', 'a',  'c') ))