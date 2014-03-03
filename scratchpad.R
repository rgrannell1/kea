
ddquote <- function (sym) {
	paste0(dQuote(sym), collapse = '')
}

Must <- local({

	this <- Object()




	this $ Be_Collection <-
		function (COLL) {
			# this macro expands to check if a value is a collection.

			COLL <- match.call()$COLL

			bquote(if (!is.atomic( .(COLL) ) && !is.list( .(COLL) ) && !is.pairlist( .(COLL) )) {

				message <-
					"the argument matching " %+% ddquote( .(COLL) ) %+%
					" must be a list, a pairlist or a typed vector." %+%
					summate( .(COLL) )

				throw(invoking_call, message)
			})
		}

	this $ Be_Fn_Matchable <-
		function (VAL) {
			# this macro expands to check if a value is a function or
			# can be looked up as a function.

			VAL <- match.call()$VAL

			bquote({if (
				!is.function( .(VAL) ) && 
				!is.name( .(VAL) ) && 
				!(is.character( .(VAL) ) && length( .(VAL) ) == 1) {

					message <-
						"the argument matching " %+% ddquote( .(VAL) ) %+%
						" must be a function, or a string or symbol naming a function." %+%
						summate( .(VAL) )

					throw(invoking_call, message)
				})
			})
		}

	this $ Be_Logical_Atom <-
		function (BOOL, PRED) {
			# this macro expands to check if a value is True, False or Na.

			BOOL <- match.call()$BOOL
			PRED <- match.call()$PRED

			bquote(if (!is.logical( .(BOOL) ) || length( .(BOOL) ) != 1) {

				message <-
					"the predicate function " %+% ddquote( .(PRED) ) %+%
					" produced a non-{True, False, Na} value." %+%
					summate( .(BOOL) )

				throw(invoking_call, message)
			})
		}


	this $ Be_Of_Length <-
		function (COLL, LENGTHS) {
			# this macro expands to check that a collection has a certain length.

			COLL <- match.call()$COLL
			LENGTHS <- match.call()$LENGTHS

			bquote(if (length( .(COLL) ) %!in% .(LENGTHS)) {

				message <- 
					"the argument matching " %+% ddquote( .(COLL) ) %+%
					"must have length" %+% paste( .(LENGTHS, collapse = ' or ') ) %+% "." %+%
					summate( .(COLL) )

				throw(invoking_call, message)
			})
		}

	this $ Be_Parametres_Of <-
		function (STRS, FN) {
			# this macro expands to check if a set of names are parametres of a function.
		}

	this $ Not_Be_Missing <-
		function (VAL) {
			# this macro expands to check if a parametre is not missing.

			VAL <- match.call()$VAL

			bquote(if (missing( .(VAL) )) {

				message <-
					"the parametre " %+% ddquote( .(VAL) ) %+%
					" is required but was missing."

				throw(invoking_call, message)
			})

		}

	this $ Not_Be_Primitive <-
		function (FN) {
			# this macro expands to check if a function is non-primitive.

			FN <- match.call()$FN

			bquote(if (is.primitive( .(FN) )) {

				message <- 
					"the argument matching " %+% ddquote( .(FN) ) %+%
					" must be a non-primitive function." %+%
					summate( .(FN) )

				throw(invoking_call, message)
			})
		}

	this $ Be_Atom <-
		MakeFun(function (VAL) {

			# MACRO( this $ Not_Be_Missing( .(VAL) ) )

		})


	this
})

test <- function (val) {
	Must $ Be_Atom(val)
}


test(1)

























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
