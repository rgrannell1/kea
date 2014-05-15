
#' xElemIs
#'
#' Test if the elements of a collection are equal to a value.
#'
#' @section Type Signature:
#'     |any| -> &lt;logical>
#'
#' @param
#'    val an arbitrary value. The value to use for testing.
#'
#' @param
#'    coll a collection. The collection to test each element of.
#'
#' @param
#'    ... see above.
#'
#' @section Corner Cases:
#'    If \bold{coll} is length-zero \bold{logical(0)} is 
#'    returned. If NA is used as \bold{val} then all forms of 
#'    NA are treated identically.
#
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemIs.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemIs
#' @export

xElemIs <- MakeFun(function (val, coll) {

	MACRO( Must $ Not_Be_Missing(val) )
	MACRO( Must $ Not_Be_Missing(coll) )
	
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else if (is.na(val)) {
		# -- treat all na's identical.
		vapply(coll, function (x) isTRUE(is.na(x)), logical(1))
	} else {
		# -- otherwise just test normally.
		vapply(coll, function (elem) {
			isTRUE(identical(elem, val))
		}, logical(1))
	}
})

#' @rdname xElemIs
#' @export

xElemIs_ <- MakeVariadic(xElemIs, 'coll')
