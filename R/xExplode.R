
#' xExplode
#'
#' Split a string into a character vector using a regular expression.
#'
#' @section Type Signature:
#'     &lt;character> -> &lt;character> -> &lt;character>
#'
#' @param
#'    rexp a regular expression. The pattern at which to
#'    split \bold{str}.
#'
#' @param
#'    str a string. The string to split.
#'
#' @return
#'    A character vector.
#'
#' @section Corner Cases:
#'    Returns the empty string if \bold{str} is length-zero.
#'    If no rexp match is found the original string is returned.
#'    Every element of the returned character vector has one
#'    or more characters - no zero-character elements are ever
#'    generated.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xExplode.R
#'
#' @rdname xExplode
#' @export

xExplode <- MakeFun('xExplode', function (rexp, str) {

	if (length(str) == 0 || length(rexp) == 0) {
		character(0)
	} else if (nchar(str) == 0) {
		''
	} else {
		str_split(rexp, str)
	}
})
