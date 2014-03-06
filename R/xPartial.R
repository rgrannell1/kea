
#' xPartial
#'
#' Fix several arguments of a function, producing a function of smaller arity.
#'
#' @details
#'     Partial application is a mechanism for specialising a general function
#'     to a particular purpose. For example, the very general function \bold{xMap}
#'     can be easily specialised to particular tasks using \code{xPartial}:
#'
#'     \code{lengths <- xPartial(xMap, list(fn = length))}
#'
#'     \code{sizes <- xPartial(xMap, object.size))}
#'
#'     In this case the use of xPartial can match the argument to specialise by
#'     name, or by position. The use of \bold{xPartial} over \bold{xPartial...} is
#'     required in the first case, to avoid a parametre name class between the argument of
#'     \bold{xMap} to fix (fn) and the function passed to \bold{xPartial} (fn).
#'
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

xPartial <- MakeFun(function (fn, coll) {
	# function -> recursive any -> any
	# partially apply a function.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)
	names_of_coll <- names(coll)

	MACRO( Must $ Be_Parametres_Of(names_of_coll, fn) )

	names(coll) <- local({

		fn_symbol <- as.symbol('fn')
		dummy_call <- as.call( as.list(c(fn_symbol, coll)) )

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
})

#' @rdname xPartial
#' @export

xPartial... <- function (fn, ...) {
	xPartial(fn, list(...))
}
