
#' xPartial
#'
#' Partially apply a function.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    A function of equal or lesser arity to \code{fn}.
#'
#' @section Corner Cases:
#'    Partial application also works for ellipses (eg list(... = 1)).
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    collection_functions
#'
#' @export

xPartial <- function (fn, coll) {
	# function -> recursive any -> any
	# partially apply a function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, profile_object(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	fn <- match.fun(fn)

	assert(
		all(names(coll) %in% xParams(fn)), invoking_call,
		exclaim$must_be_params_of(
			names(coll), fn) )

	remove(invoking_call)

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
