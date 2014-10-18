
#' xAmend
#'
#' Replace regular expression matches in a string with another string.
#'
#' @section
#'     |character| -> |character| -> |character| -> <character>
#'
#' @param
#'     rexp a string. The regular expression used to select parts
#'     of a string to replace.
#'
#' @param
#'     str1 a string. The replacement for selected parts of a string.
#'
#' @param
#'     str2 a string. The string to replace parts of.
#'
#' @return
#'     a character vector.
#'
#' @section Corner Cases:
#'      the empty character vector is returned for when \bold{rexp},
#'      \bold{str1}, or \bold{str2} is used.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xAmend.R
#'
#' @rdname xAmend
#' @export

xAmend <- MakeFun(function (rexp, str1, str2) {

	if (length(rexp) == 0 || length(str1) == 0 || length(str2) == 0) {
		character(0)
	} else {
		gsub(rexp, str1, str2)
	}
})
