
#' xMapply
#'
#' Apply a function to each element of a collection.
#'
#' @param
#'    fn a function. The function to apply to each tuple
#'    of elements in \bold{colls}.
#'
#' @param
#'    colls a collection of collections. The collection
#'    to have a function applied to each inner tuple.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list is \bold{colls} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMapply.R
#'
#' @rdname xMapply
#' @export

xMapply <- function (fn, colls) {
	# map over a collection, applying each
	# function with each tuple.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	insist $ must_be_fn_matchable(fn, invoking_call)
	insist $ must_be_collection(colls, invoking_call)

	fn <- match_fn(fn)
	if (length(colls) == 0) {
		list()
	} else {

		lapply(colls, function (tuple) {

			eval(
				as.call(c(fn, tuple)),
				envir = parent_frame)
		})

	}
}

#' @rdname xMapply
#' @export

xMapply... <- function (fn, ...) {
	xMapply(fn, list(...))
}
