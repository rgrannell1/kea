
#' xIsMatch
#'
#' Test if a string matches a regular expression.
#'
#' @section Type Signature:
#'     |character| -> |character| -> &lt;logical>
#'
#' @details
#'     \bold{xIsMatch} is roughly equivalent to \bold{grep} in
#'     base R.
#'
#'
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
#'      the empty character vector is returned for when \bold{str} is empty.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xIsMatch.R
#'
#' @rdname xIsMatch
#' @export

xIsMatch <- MakeFun(function (rexp, str) {

	MACRO( Must $ Be_Collection(rexp) )
	MACRO( Must $ Be_Collection(str) )

	str  <- as_atom(str, "character")
	rexp <- unit_to_value(as_atom(rexp, "character"))

	# -- flags like rexp are usually made into the unit.

	if (length(str) == 0) {
		logical(0)
	} else {
		isTRUE(grepl(rexp, str))
	}
})
