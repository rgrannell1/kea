
#' xSetProd
#'
#' Get the cartesian product of several collections.
#'
#' @param colls n collections.
#'
#' @return a list of n-element lists.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.

#' @family combinatoric_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xSetProd <- function (colls) {
	# set the cartesian product of n collections

	invoking_call <- sys.call()

	colls <- lapply(colls, dearrowise)

	assert(
		all( sapply(colls, function (coll) {
			is_collection(coll)
		}) ), invoking_call,
		exclaim$must_be_collection_of_length(colls))

	coll_lengths <- sapply(colls, length)

	if (length(colls) == 0 || min(coll_lengths) == 0) {
		list()
	} else {
		modulo_iths <- function (n, mods) {

			assert(n <= prod(mods), invoking_call)
			as.numeric(arrayInd(n, .dim = mods))
		}

		tuples <- vector(mode = "list", prod(coll_lengths))

		for ( ith in seq_len(prod(coll_lengths)) ) {

			indices <- modulo_iths(ith, coll_lengths)

			tuples[[ith]] <- Map(
				function (coll_ith) {
					choice <- indices[coll_ith]
					colls[[coll_ith]][[choice]]
				},
				seq_along(colls))

		}
		tuples
	}
}

#' @export

xSetProd... <- function (...) {
	xSetProd(list(...))
}
