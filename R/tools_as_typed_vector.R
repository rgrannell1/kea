

to_value_unit <- function (coll) {
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
				"internal arrow error: " %+% "
				cannot convert to non-implemented vector type.", call. = False)
		}

	} else {
		coll
	}
}

as_typed_vector <- local({

	convert_atomic_vector <- function (coll_sym, coll, mode, invoking_call) {
		# convert an atomic vector to a particular mode.

		if (length(coll) == 0) {
			vector(mode)
		} else {		
			insist $ must_be_correct_type(
				coll_sym, coll, mode, invoking_call)

			coll
		}
	}

	convert_generic_vector <- function (coll_sym, coll, mode, invoking_call) {
		# convert a generic vector to a typed vector
		# of a particular mode.

		if (length(coll) == 0) {
			vector(mode)
		} else {

			insist $ must_be_collection_of_lengths(
				coll, 1, invoking_call)

			insist $ must_be_heterogenous(
				coll_sym, coll, mode, invoking_call)

			as.vector(coll, mode = mode)
		}
	}

	function (coll, mode) {

		invoking_call <- sys.call()
		coll_sym <- match.call()$coll

		if (is.atomic(coll)) {
			convert_atomic_vector(
				coll_sym, coll, mode, invoking_call)
		} else {
			convert_generic_vector(
				coll_sym, coll, mode, invoking_call)
		}
	}

})

