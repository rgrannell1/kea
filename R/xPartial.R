
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
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xPartial <- function (fn, coll) {

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	fn <- match_fn(fn)

	assert(
		all(names(coll) %in% xParams(fn)), parent_call,
		exclaim$must_be_params_of(names(coll), fn) )

	remove(parent_call)

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

#' @export

xPartial... <- function (fn, ...) {
	xPartial(fn, list(...))
}
