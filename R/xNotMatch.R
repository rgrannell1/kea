
#' xNotMatch
#'
#' Test if a string doesn't match a regular expression.
#'
#' @section Type Signature:
#'     |character| -> |character| -> &lt;logical>
#'
#' @param
#'    rexp a string. The regular expression to test against a string.
#'
#' @param
#'    str a string. The string to check for a match.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A logical value.
#'
#' @section Corner Cases:
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xNotMatch.R
#'
#' @rdname xNotMatch
#' @export

xNotMatch <- MakeFun(function (rexp, str) {

	MACRO( Must $ Not_Be_Missing(rexp) )
	MACRO( Must $ Not_Be_Missing(str) )

	MACRO( Must $ Be_Collection(rexp) )
	MACRO( Must $ Be_Collection(str) )

	str <- as_atom(str, "character")
	rexp <- unit_to_value(as_atom(rexp, "character"))

	# -- flags like rexp are usually made into the unit.

	if (length(str) == 0) {
		logical(0)
	} else {
		isTRUE(!grepl(rexp, str))
	}
})
