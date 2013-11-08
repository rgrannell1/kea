
#' xFixDefs
#'
#' Fix function parametres with default arguments permanently.
#'
#' @param fn an arbitrary function.
#'
#' @return a function with arity equal or lesser than the arity of \code{fn}.
#'
#'
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xFixDefs <- function (fn) {
	# function -> function
	# returns a function with the default parameters
	# fixed internally.

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	fn <- match_fn(fn)
	remove(parent_call)

	do.call("function", list(
		as.pairlist( xFormals(fn)[!xHasDefs(fn)] ),
		bquote({
			# lisp-fans, rejoice!

			.(
				as.call(c(
					as.symbol('fn'),
					c(
						lapply(
							xParams(fn)[!xHasDefs(fn)],
							as.symbol),
						xFormals(fn)[xHasDefs(fn)] )
				))
			)
		})
	))
}
