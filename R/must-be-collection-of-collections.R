
Must_Be_Collection_Of_Collections <- function (COLLS) {

	COLLS <- substitute(COLLS)

	bquote({

		all_elems_are_collection <- all( vapply( .(COLLS) , function (coll) {

			'kiwi' %!in% class(coll) &&
			(is_atomic(coll) || is_generic(coll))

		}, logical(1)) )

		if (!all_elems_are_collection) {

			message <-
				"The argument matching " %+% ddquote( .(COLLS) ) %+%
				" must be a collection of lists, vectors or pairlists."

			if (any(class( .(COLLS) ) == 'kiwi')) {
				message <- message %+%
					"The argument was of class " %+% dQuote("kiwi") %+%
					". Did you use the wrong form of kiwi method (xMethod vs xMethod_)?" %+%
					summate( .(COLLS) )

			} else {
				message <- message %+% summate( .(COLLS) )
			}

			throw_kiwi_error(sys.call(), message)
		}

	})
}
