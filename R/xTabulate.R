
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
#'     The result of the tabulation is unsorted.
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
#' @rdname xTabulate
#' @export

xTabulate <- function (coll) {
	# Collection any -> Collection any
	# tabulate a collection into tuples of value: frequency

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		lapply(
			unique(coll),
			function (elem) {

				count <- 0
				for (ith in seq_along(coll)) {

					# to avoid using two measures of identity
					if ( length(unique( list(elem, coll[ith]) )) == 1 ) {
						count <- count + 1
					}
				}
				list(elem, count)
		})
	}
}

#' @rdname xTabulate
#' @export

xTabulate... <- function (...) {
	xTabulate(list(...))
}
