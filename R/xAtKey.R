
#' xAtKey
#'
#' Select a value from a collection using a key.
#'
#' @section Type Signature:
#'     |character| -> |any| -> any
#'
#' @param
#'     str a string. The key to select.
#'
#' @param
#'     coll a collection.
#'
#' @param
#'    ... see above
#'
#' @return
#'    an arbitrary value.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAtKey.R
#'
#' @family selection_functions
#'
#' @rdname xAtKey
#' @export

xAtKey <- MakeFun(function (str, coll) {

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_Collection(coll) )

	MACRO( Must $ Be_Named(coll) )

	str <- unit_to_value(as_atom(str, 'character'))

	# -- will select the first key match
	coll[[str]]
})

#' @rdname xAtKey
#' @export

xAtKey_ <- MakeVariadic(xAtKey, 'coll')
