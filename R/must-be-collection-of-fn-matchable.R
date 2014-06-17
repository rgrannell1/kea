
Must_Be_Collection_Of_Fn_Matchable <- function (COLL) {

	COLL <- substitute(COLL)

	bquote({

		all_match <- all( vapply( .(COLL) , function (val) {

			is.function(val) ||
			(is.character(val) && length(val) == 1) ||
			is.name(val)

		}, logical(1)) )

		if (!all_match) {

			message <-
				"The argument matching " %+% ddquote( .(COLL) ) %+%
				" must be a collection of functions, or symbols or strings" %+%
				" that can be looked up as functions."

			contains_kiwi <- any( vapply( .(COLL), function (elem) {
				any(class( .(COLL) ) == "kiwi")
			}, logical(1)) )

			if (contains_kiwi) {

				message <- message %+%
					"The collection supplied contained kiwi objects. " %+%
					"Did you use the wrong form of kiwi method (xMethod vs xMethod_)?" %+%
					summate( .(COLL) )

			} else {
				message <- message %+% summate( .(COLL) )
			}

			throw_kiwi_error(sys.call(), message)
		}

	})
}
