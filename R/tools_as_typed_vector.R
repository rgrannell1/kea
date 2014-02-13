
#
# unit_to_value:
#     Convert a length-zero vector to a non-length-zero value.
#
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
				"internal arrow error: " %+% "
				cannot convert to non-implemented vector type.", call. = False)
		}

	} else {
		coll
	}
}

stop('finish as atom')

as_atom <- function (coll, mode, invoking_call) {
	# convert a length one vector of any type to an atomic vector.

	invoking_call <- sys.call()
	coll_sym <- invoking_call$coll

	insist $ must_be_of_length(coll, 1, invoking_call)
	insist $ must_be_correct_type(coll_sym, coll, mode, invoking_call)
	insist $ must_be_collection_of_lengths(coll, 1, invoking_call)

	unlist(coll)
}

as_typed_vector <- function (coll, mode) {

	invoking_call <- sys.call()
	coll_sym <- invoking_call$coll

	if (length(coll) == 0) {
		vector(mode)
	} else 	if (is.atomic(coll)) {
		# atomic vector conversion.

		insist $ must_be_correct_type(
			coll_sym, coll, mode, invoking_call)

		coll

	} else {
		# generic vector conversion.

		stop("doesnt work at the moment")
		insist $ must_be_unlistable(
			coll_sym, coll, mode, invoking_call)

		as.vector(coll, mode = mode)
	}
}

