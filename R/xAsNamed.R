
#' xAsNamed
#'
#' Add names to a collection.
#'
#' @details
#' \bold{xAsNamed} is similar to the in-place assignment
#' function \bold{names<-}, except that it is not an in-place
#' assignment function.
#'
#' @param
#'    strs a collection of strings. The names to add to the
#'    input collection.
#'
#' @param
#'    coll a collection. The collection to add names to.
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero. Both
#'    duplicated and length-zero names are allowed.
#'
#' @family key_functions
#'
#' @template
#'    Variadic
#'
#' @family key_functions
#'
#' @example
#'    inst/examples/example-xAsNamed.R
#'
#' @rdname xAsNamed
#' @export

xAsNamed <- MakeFun(function (strs, coll) {
	# Vector string -> Collection any -> [any]
	# add names to a collection.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(strs) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(strs) )
	MACRO( Must $ Be_Collection(coll) )

	strs <- as_typed_vector(strs, 'character')

	MACRO( Must $ Be_Equal_Length_To(strs, coll) )

	names(coll) <- strs
	coll
})
