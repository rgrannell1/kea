
#' @param fn an arg
#'
#' @export

xFixDefaults <- function (fn) {
	# function -> function
	# returns a function with the default parameters
	# fixed internally.

	pcall <- sys.call()
	require_a('functionable', fn, pcall)

	fn <- match.fun(fn)
	remove(pcall)

	do.call("function", list(
		as.pairlist( xFormals(fn)[!xHasDefaults(fn)] ),
		bquote({
			# lisp-fans, rejoice!

			.( 
				as.call(c(
					as.symbol('fn'),
					c(
						lapply(
							xParams(fn)[!xHasDefaults(fn)],
							as.symbol),
						xFormals(fn)[xHasDefaults(fn)] )
					)) 
			)
		})
	))
}