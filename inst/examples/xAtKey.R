
#' xAtKey
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

xAtKey <- MakeFun(function (str, coll) {
	# string -> Collection any -> any

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(str) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_Collection(coll) )

	str <- unit_to_value(as_atom(str, 'character'))

	coll[[str]]
})

#' @rdname xAtKey
#' @export

xAtKey... <- function (str, ...) {
	xAtKey(str, list(...))
}
