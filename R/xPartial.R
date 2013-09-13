
#' xPartial
#' 
#' Partially apply a function.
#'
#' @param fn an arbitrary function.
#' @param coll a collection
#'
#' @return A function of equal or lesser arity to \code{fn}.
#'
#' @section Corner Cases: 
#'     Partial application also works for ellipses (eg list(... = 1)).
#' @template glossary
#'
#' @examples 
#' @export

xPartial <- function (fn, coll) {

	pcall <- sys.call()

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	fn <- match.fun(fn)
	remove(pcall)

	assert( all(names(coll) %in% xParams(fn)), pcall)

	if (length(coll) == 0) {
		fn
	} else {

		do.call("function", list(
			as.pairlist( xFormals(fn)[
				!(xParams(fn) %in% names(coll)) ] ),
			bquote({
				# the fundemental unit of lisp-like 
				# computation; LE PARENTHESIS!

				.( 
					as.call(c(
						as.symbol('fn'),
						lapply(
							xParams(fn),
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

