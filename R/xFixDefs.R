
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
#' @examples 

#' @export

xFixDefs <- function (fn) {
	# function -> function
	# returns a function with the default parameters
	# fixed internally.

	pcall <- sys.call()

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)
	
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