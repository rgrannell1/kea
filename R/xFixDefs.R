
#' xFixDefs
#' 
#' Fix function parametres with default arguments permanently.
#'
#' @param fn an arbitrary function.
#'
#' @return a function with arity equal or lesser than the arity of \code{fn}.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R

#' @export

xFixDefs <- function (fn) {
	# function -> function
	# returns a function with the default parameters
	# fixed internally.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))
	
	fn <- match.fun(fn)
	remove(pcall)

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