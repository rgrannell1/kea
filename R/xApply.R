
#' xApply
#'
#' Invoke a function with a collection of arguments.
#'
#' @param
#'    fn an function of any arity. The function to
#'    call.
#'
#' @param
#'    coll a collection. The arguments to pass to
#'    \bold{fn}. The collection may be named
#'    or unnamed; normal R function call semantics
#'    dictate how the arguments are used by the input function.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The return value of \bold{fn}.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xApply.R
#'
#' @rdname xApply
#' @export

xApply <- function (fn, coll) {
	# function -> [any] -> any
	# call the function fn with the list coll.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_fn_matchable(fn, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	fn <- match_fn(fn)

	try_hof(
		eval(
			as.call(c(fn, coll)),
			envir = parent_frame),
		invoking_call)
}

#' @rdname xApply
#' @export

xApply... <- function (fn, ...) {
	xApply(fn, list(...))
}
