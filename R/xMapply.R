
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

xMapply <- MakeFun(function (fn, colls) {

	MACRO( Fix(fn, colls) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(colls) )
	MACRO( Must $ Be_Collection_Of_Collections(colls) )

	fn <- match_fn(fn)

	if (length(colls) == 0) {
		list()
	} else {

		lapply(colls, function (tuple) {
			do.call(fn, tuple)
		})
	}
})

#' @rdname xMapply
#' @export

xMapply_ <- MakeVariadic(xMapply, 'colls')
