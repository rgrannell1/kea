
Must_Be_Collection_Of_Collections <- function (COLLS) {

	COLLS <- substitute(COLLS)

	bquote({

		all_elems_are_collection <- all( vapply( .(COLLS) , function (coll) {

			'kea' %not_in% class(coll) &&
			(is_atomic(coll) || is_generic(coll))

		}, logical(1)) )

		if (!all_elems_are_collection) {

			message <-
				"The argument matching " %+% ddquote( .(COLLS) ) %+%
				" must be a collection of lists, vectors or pairlists."

			if (any(class( .(COLLS) ) == 'kea')) {
				message <- message %+%
					"The argument was of class " %+% dQuote("kea") %+%
					". Did you use the wrong form of kea method (xMethod vs xMethod_)?" %+%
					summate( .(COLLS) )

			} else {
				message <- message %+% summate( .(COLLS) )
			}

			throw_exception $ type_error(sys.call(), message)
		}

	})
}
