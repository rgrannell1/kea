
#' xFormals
#'
#' Get the parametres and defaults of a function or primitive function.
#'
#' @param fn an arbitrary function.
#'
#' @return A named list, where each element's name is a parameter in f and each value
#'	 is the default value of that parameter.
#'
#' @section Corner Cases:
#'	 If \code{fn} is a primitive function a heuristic is used to obtain
#'	 its formals. If a parameter has no default, the value of the
#'	 corresponding element in the retun value will be the empty symbol,
#'	 which is identical to \code{quote(expr=)}.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xFormals <- function (fn) {
	# (a -> b) -> [a, b]
	# get the formals of non-primitive functions, and
	# the arguments of primitive functions.

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	fn <- match_fn(fn)

	if (is.primitive(fn)) {
		as.list( head(as.list(args(fn)), -1) )
	} else {
		as.list( formals(fn) )
	}
}
