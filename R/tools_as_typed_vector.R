
#
# unit_to_value:
#     Convert a length-zero vector to a non-length-zero value.
#
#
#
# as_atom:
#      Convert a length-one vector to an atomic vector.
#
#
#
# as_typed_vector:
#     A function to try convert a list of values to a typed vector.
#     A list of integers should be interconvertable to an integer vector,
#     if required. This function is used to make sure arrow functions are
#     agnostic to the difference between typed and generic vectors.

unit_to_value <- function (coll) {
	# collection -> collection
	# convert a length-zero collection.

	if (length(coll) == 0) {
		if (is.double(coll) || is.integer(coll)) {
			0
		} else if (is.character(coll)) {
			""
		} else if (is.logical(coll)) {
			False
		} else if (is.raw(coll)) {
			as.raw(00)
		} else if (is.complex(coll)) {
			0 + 0i
		} else {
			stop(
				"internal arrow error: cannot convert to non-implemented vector type." %+%
				"please report this at the arrow github repo!",
				call. = False)
		}

	} else {
		coll
	}
}

as_typed_vector <- local({

	# -- is.na is essential; all na's are treated the same.

	typecheck <- list(
		numeric =
			# -- is.numeric checks if integer or double
			function (x) is.numeric(x) || is.na(x),
		integer =
			function (x) is.integer(x) || is.na(x),
		double =
			function (x) is.double(x) || is.na(x),
		character =
			function (x) is.character(x) || is.na(x),
		logical =
			function (x) is.logical(x) || is.na(x),
		complex =
			function (x) is.complex(x) || is.na(x),
		raw =
			function (x) is.raw(x) || is.na(x)
	)

	function (coll, mode) {

		# -- important! this captures the invoking call.
		invoking_call <- sys.call(-1)

		if (length(coll) == 0) {
			vector(mode)
		} else if (is.atomic(coll)) {
			# -- this branch is faster; check the vector is the correct type.

			type <- typeof(coll)

			if (mode == 'numeric') {
				# -- integers, doubles, numerics are all valid numerics.

				if ( !any(type == c('integer', 'double', 'numeric')) ) {

					coll_sym <- match.call()$coll

					message <- "the collection " %+% dQuote(coll_sym) %+%
						"must be a collection of values of type " %+% mode %+% "."

					throw_arrow_error(
						invoking_call, message)
				}

				coll
			} else if (!type == mode) {
				# -- otherwise the type has to be the mode.

				coll_sym <- match.call()$coll

				message <- "the collection " %+% dQuote(coll_sym) %+%
					"must be a collection of values of type " %+% mode %+% "."

				throw_arrow_error(
					invoking_call, message)
			}

			coll
		} else {
			# -- check that the generic vector is correct mode; the slow path.

			is_correct_type <- typecheck[[mode]]

			for (elem in coll) {

				# -- check the element is length-one
				if (length(elem) != 1) {

					coll_sym <- match.call()$coll

					throw_arrow_error(
						invoking_call, "the collection " %+% dQuote(coll) %+%
						" must be a collection of length-one elements." %+%
						summate(coll ))
				}

				# -- check the element is a vector of the correct type.
				if (!is_correct_type(elem)) {

					coll_sym <- match.call()$coll

					message <- "the collection " %+% dQuote(coll_sym) %+%
						"must be a collection of values of type " %+% mode %+% "."

					if (any(class(coll) == 'arrow')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("arrow") %+%
							". Did you use the wrong form of arrow method (xMethod vs xMethod_)?" %+%
							summate(coll)
					} else {
						message <- message %+% summate(coll)
					}

					throw_arrow_error(
						invoking_call, message)
				}
			}

			as.vector(coll, mode = mode)
		}
	}
})



as_atom <- local({

	# -- is.na is essential; all na's are treated the same.

	typecheck <- list(
		numeric =
			# -- is.numeric checks if integer or double
			function (x) is.numeric(x) || is.na(x),
		integer =
			function (x) is.integer(x) || is.na(x),
		double =
			function (x) is.double(x) || is.na(x),
		character =
			function (x) is.character(x) || is.na(x),
		logical =
			function (x) is.logical(x) || is.na(x),
		complex =
			function (x) is.complex(x) || is.na(x),
		raw =
			function (x) is.raw(x) || is.na(x)
	)

	function (coll, mode) {

		# -- important! this captures the invoking call.
		invoking_call <- sys.call(-1)

		if (length(coll) == 0) {
			vector(mode)
		} else if (length(coll) != 1) {

			coll_sym <- match.call()$coll

			throw_arrow_error(
				invoking_call, "the collection " %+% dQuote(coll_sym) %+%
				" must be a length-one value." %+% summate(coll)
			)

		} else if (is.atomic(coll)) {
			# -- fast track; check if the atomic vector is atomic.

			type <- typeof(coll)

			if (mode == 'numeric') {
				# -- integers, doubles, numerics are all valid numerics.

				if ( !any(type == c('integer', 'double', 'numeric')) ) {

					coll_sym <- match.call()$coll

					message <- "the collection " %+% dQuote(coll_sym) %+%
						"must be a collection of values of type " %+% mode %+% "."

					if (any(class(coll) == 'arrow')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("arrow") %+%
							". Did you use the wrong form of arrow method (xMethod vs xMethod_)?" %+%
							summate(coll)
					} else {
						message <- message %+% summate(coll)
					}

					throw_arrow_error(
						invoking_call, message)
				}

				coll
			} else if (!type == mode) {
				# -- check that the expected type

				coll_sym <- match.call()$coll

				message <- "the collection " %+% dQuote(coll_sym) %+%
					"must be a collection of values of type " %+% mode %+% "."

				if (any(class(coll) == 'arrow')) {
					message <- message %+%
						"The argument was of class " %+% dQuote("arrow") %+%
						". Did you use the wrong form of arrow method (xMethod vs xMethod_)?" %+%
						summate(coll)
				} else {
					message <- message %+% summate(coll)
				}

				throw_arrow_error(
					invoking_call, message)
			}

			coll
		} else {
			# -- generic vectors; more work needed.

			is_correct_type <- typecheck[[mode]]

			for (elem in coll) {

				if (length(elem) != 1) {

					coll_sym <- match.call()$coll

					throw_arrow_error(
						invoking_call, "the collection " %+% dQuote(coll_sym) %+%
						" must be a collection of length-one elements." %+%
						summate(coll ))
				}
				if (!is_correct_type(elem)) {

					coll_sym <- match.call()$coll

				message <- "the collection " %+% dQuote(coll_sym) %+%
						"must be a collection of values of type " %+% mode %+% "."

					if (any(class(coll) == 'arrow')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("arrow") %+%
							". Did you use the wrong form of arrow method (xMethod vs xMethod_)?" %+%
							summate(coll)
					} else {
						message <- message %+% summate(coll)
					}

					throw_arrow_error(
						invoking_call, message)
				}
			}

			# -- convert. This might be replacable with coll[[ 1 ]]
			as.vector(coll, mode = mode)
		}
	}
})

