
#' xParamsOf
#'
#' Get the parametre names of a function or primitive function.
#'
#' @param
#'   fn an arbitrary function or primitive function. The function to
#'   find the parametre names of.
#'
#' @return
#'   A character vector.
#'
#' @section Corner Cases:
#'	 If \bold{fn} is a primitive function a heuristic is used to obtain
#'	 its parametre names, which may not work for all functions. If a function
#'   has no parametres the empty character vector is returned.
#'
#' @family parametre_functions
#'
#' @example
#'    inst/examples/example-xParamsOf.R
#'
#' @rdname xParamsOf
#' @export

xParamsOf <- function (fn) {
	# function -> Vector string
	# get the formals of non-primitive functions, and
	# the arguments of primitive functions.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(fn)

	insist $ must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)

	formals_fn <- if (is.primitive(fn)) {
		as.list( head(as.list(args(fn)), -1) )
	} else {
		as.list( formals(fn) )
	}

	if (length(formals_fn) == 0) {
		character(0)
	} else {
		names(formals_fn)
	}
}
