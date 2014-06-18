
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
#' @section Corner Cases:
#'    If \bold{str} occurs multiple times in \bold{coll}
#'    the first match is returned.
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

	MACRO( Must_Be_Named(coll) )

	# -- avoid violating the type signature.
	MACRO( Must_Be_Longer_Than(0, str) )

	# -- will select the first key match
	coll[[str]]
})

#' @rdname xAtKey
#' @export

xAtKey_ <- MakeVariadic(xAtKey, 'coll')
