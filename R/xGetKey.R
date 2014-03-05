
#' xGetKey
#'
#' Return a function that selects a key from a collection.
#'
#' @param
#'    str a string. The key to select from a collection.
#'
#' @return
#'    A unary function that takes a collection.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @family key_functions
#'
#' @example
#'    inst/examples/example-xGetKey.R
#'
#' @rdname xGetKey
#' @export

xGetKey <- MakeFun(function (str) {
	# Vector string -> (Collection -> [any])
	# Return a function that selects a key from a collection.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(str) )

	MACRO( arrow ::: Must $ Be_Collection(str) )

	str <- unit_to_value(as_atom(str, 'character'))

	function (coll) {
		"A function created by xGetKey."
		""
		coll[[str]]
	}
})
