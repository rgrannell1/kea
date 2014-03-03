
ddquote <- function (sym) {
	paste0(dQuote(sym), collapse = '')
}

Must <- local({

	this <- Object()

	this $ Not_Be_Missing <-
		function (VAL) {
			# this macro expands to check if a parametre is missing.

			VAL <- match.call()$VAL

			bquote(if (missing( .(VAL) )) {

				message <-
					"the parametre " %+% ddquote( .(VAL) ) %+%
					" is required but was missing."

				throw(invoking_call, message)
			})

		}

	this $ Be_Collection <-
		function (COLL) {

			COLL <- match.call()$COLL

			bquote(if (!is.atomic( .(COLL) ) && !is.list( .(COLL) ) && !is.pairlist( .(COLL) )) {

				message <-
					"the argument matching " %+% ddquote( .(COLL) ) %+%
					" must be a list, a pairlist or a typed vector." %+%
					summate( .(COLL) )

				throw(invoking_call, message)
			})
		}



	this
})






























MakeFun <- function (expr) {

	unquote <- function (inner) {

		if (is.pairlist(inner)) {
			as.pairlist(lapply(inner, unquote))
		} else if (length(inner) <= 1L) {
			inner
		} else if (inner[[1L]] == as.name("MACRO")) {
			eval(inner[[2L]], parent.frame())
		} else {
			as.call(lapply(inner, unquote))
		}
	}

	eval(unquote(substitute(expr)), parent.frame())
}




MakeFun(function (pred, coll) {

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(pred) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(coll) )

	pred <- match.fun(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- vapply(coll, pred, logical(1), USE.NAMES = FALSE)

		as.list( coll[ !is.na(ind) & ind ] )
	}

})
