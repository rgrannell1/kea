
#' xMapply
#'
#' Apply a function to each element of a collection.
#'
#' @section Type Signature:
#'     (..any -> any) -> ||any|| -> |any|
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

xMapply <- MakeFun('xMapply', function (fn, colls) {

	if (length(colls) == 0) {
		keep_names(list(), colls)
	} else {

		lapply(colls, function (tuple) {
			# -- apply each inner tuple.
			do.call(fn, as.list(tuple))
		})
	}
})

#' @rdname xMapply
#' @export

xMapply_ <- MakeVariadic(xMapply, 'colls')
