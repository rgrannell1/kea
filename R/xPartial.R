
xPartial <- function (fn, coll) {

	pcall <- sys.call()
	require_a('functionable', fn, pcall)
	require_a('collection', coll, pcall)

	fn <- match.fun(fn)
	remove(pcall)

	stopifnot( all(names(coll) %in% xParameters(fn)) )
	stopifnot( length(unique(coll)) == length(coll) )

	if (length(coll) == 0) {
		fn
	} else {

		do.call("function", list(
			as.pairlist( xFormals(fn)[
				!(xParameters(fn) %in% names(coll)) ] ),
			bquote({
				# the fundemental unit of lisp-like 
				# computation; LE PARENTHESIS!

				.( 
					as.call(c(
						as.symbol('fn'),
						lapply(
							xParameters(fn),
							function (param) {
								if (param %in% names(coll)) {
									coll[[param]]
								} else {
									as.symbol(param)
								}
							}) )) )
			})
		))
	}
}

