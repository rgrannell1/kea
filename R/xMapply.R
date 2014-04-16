
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
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMapply.R
#'
#' @rdname xMapply
#' @export

xMapply <- MakeFun(function (fn, colls) {
	# map over a collection, applying each
	# function with each tuple.

	parent_frame <- parent.frame()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(colls) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(colls) )
	MACRO( Must $ Be_Collection_Of_Collections(colls) )

	fn <- match_fn(fn)

	if (length(colls) == 0) {
		list()
	} else {

		lapply(colls, function (tuple) {
			eval(as.call(c(fn, tuple)), envir = parent_frame)
		})

	}
})

#' @rdname xMapply
#' @export

xMapply... <- function (fn, ...) {
	xMapply(fn, list(...))
}
