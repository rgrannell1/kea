
#' xApply
#'
#' Invoke a function with a collection of arguments.
#'
#' @section Uses:
#'    \code{xApply} is an adaptor function that allows variadic functions
#'    to be called with a variable number of arguments at runtime.
#'    For example, if \code{xJoin} is invoked directly it has
#'    to be given a fixed number of lists to concatenate.
#'    If \code{xJoin} is invoked with \code{xApply} then
#'    xJoin can be invoked with any number of lists dynamically.
#'
#' @param
#'    fn an function of any arity.
#'
#' @param
#'    coll a list or pairlist. This may be named, but
#'    all names must be parametres of \code{fn}.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    the return value of \code{fn}.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @template
#'    Glossary
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

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	fn <- match.fun(fn)

	try_higher_order(
		eval(
			as.call(c(fn, coll)),
			envir = parent_frame),
		invoking_call)
}

#'
#' @rdname xApply
#' @export

xApply... <- function (fn, ...) {
	xApply(fn, list(...))
}
