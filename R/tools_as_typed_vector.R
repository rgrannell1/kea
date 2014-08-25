
as_typed_vector <- local({

	# -- is_na is essential; all na's are treated the same.

	typecheck <- list(
		numeric =
			# -- is.numeric checks if integer or double efficiently
			# -- na check is less efficent; make second.
			# -- is_na checks if ANY type of na.
			# -- not using vectorisation;
			function (x) is.numeric(x),
		integer =
			function (x) is.integer(x),
		double =
			function (x) is.double(x),
		character =
			function (x) is.character(x),
		logical =
			function (x) is.logical(x),
		complex =
			function (x) is.complex(x),
		raw =
			function (x) is.raw(x)
	)

	function (coll, mode) {

		# -- important! this captures the correct invoking call.
		invoking_call <- sys.call(-1)

		if (length(coll) == 0) {
			# -- length-zero atomic or generic pathway.
			# -- fast.

			if ( any(typeof(coll) == c('integer', 'double')) && mode == 'numeric' ) {
				coll
			} else if ( !is.null(names(coll)) ) {
				structure(vector(mode), names = character(0))
			} else {
				vector(mode)
			}

		} else if (is.atomic(coll)) {
			# -- length-one or larger.
			# -- this branch is fast; check the vector is the correct type.

			type      <- typeof(coll)

			# -- every elem must be NA for conversion.
			all_is_na <- all(elem_is_na(coll))

			if (mode == 'numeric') {
				# -- double or integer path.

				# -- this checks if the collection is integer, double, or
				# -- all na values. Conversion is always possible for all NA values.

				if (type != 'double' && type != 'integer' && !all_is_na) {

					coll_sym <- substitute(coll)

					message <- "the collection " %+% dQuote(coll_sym) %+%
						" must be a collection of values of type " %+% mode %+% "."

					throw_kea_error(invoking_call, message)
				}

			} else if (mode == 'raw' && type != 'raw') {
				# -- raws don't have NA values.

				coll_sym <- substitute(coll)

				message <- "the collection " %+% dQuote(coll_sym) %+%
					" must be a collection of values of type " %+% mode %+% "."

				throw_kea_error(invoking_call, message)

			} else if (!typecheck [[mode]] (coll) && !all_is_na) {
				# -- other types pathway.

				coll_sym <- substitute(coll)

				message <- "the collection " %+% dQuote(coll_sym) %+%
					" must be a collection of values of type " %+% mode %+% "."

				throw_kea_error(invoking_call, message)
			}

			# -- convert in case vector was all NA values.

			if (all_is_na) {

				class(coll) <- mode
				coll

			} else {
				coll
			}

		} else {
			# -- generic vectors.
			# -- check that the generic vector can be converted to a vector of the right mode;
			# -- very slow.

			is_correct_type <- typecheck[[mode]]

			for (elem in coll) {

				# -- check the element is length-one.
				if (length(elem) != 1) {

					coll_sym <- substitute(coll)

					throw_kea_error(
						invoking_call, "the collection " %+% dQuote(coll_sym) %+%
						" must be a collection of length-one elements." %+%
						summate(coll ))
				}

				# -- check the element is a vector of the correct type.
				if (!is_correct_type(elem)) {

					coll_sym <- substitute(coll)

					message <- "the collection " %+% dQuote(coll_sym) %+%
						" must be a collection of values of type " %+% mode %+% "."

					if (any(class(coll) == 'kea')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("kea") %+%
							". Did you use the wrong form of kea method (xMethod vs xMethod_)?" %+%
							summate(coll)
					} else {
						message <- message %+% summate(coll)
					}

					throw_kea_error(invoking_call, message)
				}
			}

			if ( !is.null(names(coll)) ) {
				out <- as.vector(coll, mode = mode)
				names(out) <- names(coll)
				out
			} else {
				as.vector(coll, mode = mode)
			}

		}
	}
})









# -- the argument given to 'as_atom' must be given as a variable;
# -- otherwise the displayed source is screwed up.

as_atom <- local({

	# -- is_na is essential; all na's are treated the same.

	typecheck <- list(
		numeric =
			function (x) is.numeric(x)   || all(elem_is_na(x)),
		integer =
			function (x) is.integer(x)   || all(elem_is_na(x)),
		double =
			function (x) is.double(x)    || all(elem_is_na(x)),
		character =
			function (x) is.character(x) || all(elem_is_na(x)),
		logical =
			function (x) is.logical(x)   || all(elem_is_na(x)),
		complex =
			function (x) is.complex(x)   || all(elem_is_na(x)),
		raw =
			function (x) is.raw(x)       || all(elem_is_na(x))
	)

	function (coll, mode) {

		# -- important! this captures the invoking call.
		invoking_call <- sys.call(-1)

		if (length(coll) == 0) {
			if ( !is.null(names(coll)) ) {
				structure(vector(mode), names = character(0))
			} else {
				vector(mode)
			}
		} else if (length(coll) != 1) {

			coll_sym <- substitute(coll)

			throw_kea_error(
				invoking_call, "the collection " %+% dQuote(coll_sym) %+%
				" must be a length-one value." %+% summate(coll)
			)

		} else if (is.atomic(coll)) {
			# -- fast track; check if the atomic vector is atomic.

			if (mode == 'numeric') {
				# -- integers, doubles, numerics are all valid numerics.

				# -- needed for NA to be untyped.
				if (!typecheck $ integer(coll) && !typecheck $ double(coll)) {

					coll_sym <- substitute(coll)

					message <- "the collection " %+% dQuote(coll_sym) %+%
						" must be a collection of values of type " %+% mode %+% "."

					if (any(class(coll) == 'kea')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("kea") %+%
							". Did you use the wrong form of kea method (xMethod vs xMethod_)?" %+%
							summate(coll)
					} else {
						message <- message %+% summate(coll)
					}

					throw_kea_error(
						invoking_call, message)
				}

				coll

			} else if (!typecheck [[mode]] (coll)) {
				# -- check that the expected type

				coll_sym <- substitute(coll)

				message <- "the collection " %+% dQuote(coll_sym) %+%
					" must be a collection of values of type " %+% mode %+% "."

				if (any(class(coll) == 'kea')) {
					message <- message %+%
						"The argument was of class " %+% dQuote("kea") %+%
						". Did you use the wrong form of kea method (xMethod vs xMethod_)?" %+%
						summate(coll)
				} else {
					message <- message %+% summate(coll)
				}

				throw_kea_error(
					invoking_call, message)
			}

			coll

		} else {
			# -- generic vectors; more work needed.

			is_correct_type <- typecheck[[mode]]

			for (elem in coll) {

				if (length(elem) != 1) {

					coll_sym <- substitute(coll)

					throw_kea_error(
						invoking_call, "the collection " %+% dQuote(coll_sym) %+%
						" must be a collection of length-one elements." %+%
						summate(coll ))
				}
				if (!is_correct_type(elem)) {

					coll_sym <- substitute(coll)

				message <- "the collection " %+% dQuote(coll_sym) %+%
						" must be a collection of values of type " %+% mode %+% "."

					if (any(class(coll) == 'kea')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("kea") %+%
							". Did you use the wrong form of kea method (xMethod vs xMethod_)?" %+%
							summate(coll)
					} else {
						message <- message %+% summate(coll)
					}

					throw_kea_error(
						invoking_call, message)
				}
			}

			# -- convert. This might be replacable with coll[[ 1 ]]

			if ( !is.null(names(coll)) ) {
				out <- as.vector(coll, mode = mode)
				names(out) <- names(coll)
				out
			} else {
				as.vector(coll, mode = mode)
			}

		}
	}
})
