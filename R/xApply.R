
#' xApply
#'
#' Invoke a function with a collection of arguments.
#'
#' @details
#'    \bold{xApply} is an adaptor function that allows any function
#'    to be called with a collection of arguments. A typical use case
#'    is to allow a function with ellipsis (...) arguments to be called with
#'    a variable number of arguments.
#'
#'    \code{coll <- list( list(1, 2), list(3, 4), list(5, 6) )}
#'
#'    \code{xApply(rbind, coll)}
#'
#'    In the above case the function rbind is called on several rows, without
#'    having to use reduce or a for loop to successively bind the rows.
#'
#'    It isn't usually necessary to call arrow functions with \bold{xApply},
#'    since they exist with both variadic and non-variadic forms.
#'
#' @param
#'    fn an function of any arity. The function to call.
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

xApply <- MakeFun(function (fn, coll) {
	# function -> [any] -> any
	# call the function fn with the list coll.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	MACRO( arrow ::: Must $ Not_Be_Missing(fn) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(fn) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	try_hof(
		eval(
			as.call(c(fn, coll)),
			envir = parent_frame),
		invoking_call)
})

#' @rdname xApply
#' @export

xApply... <- function (fn, ...) {
	xApply(fn, list(...))
}
