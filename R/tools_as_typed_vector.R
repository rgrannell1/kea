
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
		} else {
			write_error(
				"internal arrow error: cannot convert to non-implemented vector type.",
				call. = False)
		}

	} else {
		coll
	}
}

as_atom <- function (coll, mode, invoking_call) {
	# convert a length one vector of any type to an atomic vector.

	invoking_call <- sys.call()

	insist $ must_be_atom(coll, invoking_call)

	if (length(coll) == 0) {
		vector(mode)
	} else {
		coll <- coll[[1]]
		insist $ must_be_correct_type(
			invoking_call$coll, coll, mode, invoking_call)

		coll[[1]]
	}
}

as_typed_vector <- local({

	check_valid <- function (elem, mode) {
		if (length(elem) != 1) {
			stop("")
		}
		if (mode(elem) != mode) {
			stop("")
		}
	}

	typecheck <- list(
		numeric =
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

		invoking_call <- sys.call()

		if (length(coll) == 0) {
			vector(mode)
		} else if (is.atomic(coll)) {

			type <- typeof(coll)

			if (mode == 'numeric') {

				if (type %!in% c('integer', 'double', 'numeric')) {

					coll_sym <- match.call()$coll

					throw_arrow_error(
						invoking_call, "the collection " %+% ddquote(coll_sym) %+%
						"must be a collection of values of type " %+% mode %+%
						summate(coll))
				}

				coll
			} else if (!type == mode) {

				coll_sym <- match.call()$coll

				throw_arrow_error(
					invoking_call, "the collection " %+% ddquote(coll_sym) %+%
					"must be a collection of values of type " %+% mode %+% "." %+%
					summate(coll))
			}

			coll
		} else {

			is_correct_type <- typecheck[[mode]]

			for (elem in coll) {

				if (length(elem) != 1) {

					coll_sym <- match.call()$coll

					throw_arrow_error(
						invoking_call, "the collection " %+% ddquote(coll) %+%
						" must be a collection of length-one elements." %+%
						summate(coll ))
				}
				if (!is_correct_type(elem)) {

					coll_sym <- match.call()$coll

					throw_arrow_error(
						invoking_call, "the collection " %+% ddquote(coll_sym) %+%
						" must be a collection of values of type " %+% mode %+% "." %+%
						summate(coll))
				}
			}

			as.vector(coll, mode = mode)
		}
	}
})



as_atom <- as_typed_vector
