
#' xNotMatch
#'
#' Test if a string doesn't match a regular expression.
#'
#' @section Type Signature:
#'     |character| -> |character| -> <logical>
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
#'    inst/examples/example-xNotMatch.R
#'
#' @rdname xNotMatch
#' @export

xNotMatch <- MakeFun(function (rexp, str) {

	if (length(rexp) == 0 || length(str) == 0) {
		logical(0)
	} else {
		isTRUE(!grepl(rexp, str))
	}
})
