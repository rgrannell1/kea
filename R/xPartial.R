
#' xPartial
#'
#' Partially apply a function.
#'
#' @param
#'    fn an arbitrary function. The function to have some
#'    of its arguments fixed.
#'
#' @param
#'    coll a collection. The arguments to fix.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A function of equal or lesser arity to \bold{fn}.
#'
#' @section Corner Cases:
#'    Partial application also works for ellipses (eg list(... = 1)).
#'
#' @family function_modifying_functions
#'
#' @family parametre_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPartial.R
#'
#' @rdname xPartial
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

	insist $ must_be_fn_matchable(fn, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	fn <- match_fn(fn)
	insist $ must_be_parametres_of(names(coll), fn, invoking_call)

	names(coll) <- local({

		fn_symbol <- as.symbol('fn')

		dummy_call <- as.call( as.list(c(fn_symbol, coll)) )

		print(fn)

		matched_call <- match.call(fn, call = dummy_call)
		names(matched_call[-1])
	})

	remove(invoking_call)

	if (length(coll) == 0) {
		fn
	} else {

		do.call("function", list(
			as.pairlist( xFormalsOf(fn)[
				!(xParamsOf(fn) %in% names(coll)) ] ),
			bquote({
				"a function returned by xPartial."
				""
				# the fundemental unit of lisp-like
				# computation; LE PARENTHESIS!
				# for the love of god, do not ask how this code works,
				# it just does, in the kind of way that if you looked at it sideways
				# it would fall apart, parenthetically he said.

				.(
					as.call(c(
						as.symbol('fn'),
						lapply(
							xParamsOf(fn),
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

#' @rdname xPartial
#' @export

xPartial... <- function (fn, ...) {
	xPartial(fn, list(...))
}
