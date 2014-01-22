
# To Developers,
#
# assert:
#     Assert checks if a proposition is true, and if it fails
#     throws a helpful error (see say.R).


format_call <- function (call) {
	# call -> string
	# format the call nicely for printing, fixing the representation of ':='.

	call <- as.call(lapply(call, function (term) {

		if (is.call(term) && term[[1]] == as.symbol(':=')) {
			eval(term)
		} else {
			term
		}

	}) )

	ddparse(call)
}

# --------------------- assertion functions --------------------- #
#

assert <- local({

	consts <- list(
		margin =
			80
	)

	function (expr, invoking_call, message) {
		# does an expression evaluate to true?
		# if not, throw a lovely error.

		args <- as.list(match.call())[-1]
		this_call <- sys.call()

		if (!is.logical(expr)) {
			stop(
				yelp$non_logical_assertion(expr),
				call. = False)
		}

		if (!isTRUE(expr)) {
			# everythings went wrong, throw an error.

			callname <- paste0(invoking_call[[1]], collapse = '')

			call <- local({

				calltext <- format_call(invoking_call)

				if (nchar(calltext) > consts$margin) {

					paste0(
						substr(calltext, 1, consts$margin), '...',
						collapse = '')

				} else {
					calltext
				}
			})

			stop(
				yelp$arrow_function_failed(
					callname, call, message),
				call. = False)

		}

		invisible(Null)
	}

})

# -------------------------------- insist -------------------------------- #
#
# To Developers,
# insist is a list of functions that provide a minimal interface to an
# assertion. This object exists to reduce the amount of assertion checking
# code needed.
#
#     insist$must_be_fn_matchable(fn, invoking_call)
#
#

insist <- local({

	this <- Object()

	#  -------- value -------- #

	this$must_be_of_length <-
		function (val, lengths, invoking_call) {
			# the value must have a length in the set of lengths.

			val_sym <- match.call()[-1][[1]]
			lengths <- paste(lengths, collapse = " or ")

			message <- "the argument matching " %+% dQuote(val_sym) %+%
				" must have length " %+% lengths %+% "." %+%
				summate(val)

			assert(
				length(val) %in% lengths, invoking_call,
				message)
		}

	#  -------- functions -------- #

	this$is_logical_result <-
		function (result, pred, invoking_call) {
			# predicates must return logical values.

			pred_sym <- match.call()[-1][[1]]

			message <- "the predicate function " %+% dQuote(pred_sym) %+%
				" produced a non-logical value." %+%
				summate(pred)

			assert(
				is.logical(result), invoking_call, message)

		}

	this$must_be_non_primitive <-
		function (fn, invoking_call) {
			# the function must be non primitive function.

			fn_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(fn_sym) %+%
				" must be a non-primitive function." %+%
				summate(fn)

			assert(
				!is.primitive(fn), invoking_call,
				message)
		}

	this$must_be_fn_matchable <-
		function (fn, invoking_call) {
			# the value must be lookupable as a function.

			fn_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(fn_sym) %+%
				" must be a function, or a symbol or string" %+%
				" that can be looked-up as a function. "%+%
				summate(fn)

			assert(
				is_fn_matchable(fn),
				invoking_call, message)
		}

	this$must_be_parametres_of <-
		function (names, fn, invoking_call) {
			# the names given must be parametre names
			# of the function given.

			names_sym <- match.call()[-1][[1]]
			fn_sym <- match.call()[-1][[2]]

			message <- "the elements of " %+% names_sym %+%
				" must be parametre names of " %+% fn_sym %+% "." %+%
				summate(names)

			assert(
				all(names %in% xParamsOf(fn)), invoking_call,
				message)
		}

	#  -------- character -------- #

	this$must_be_character <-
		function (strs, invoking_call) {
			# the value must be a character vector.

			strs_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(strs_sym) %+%
				" must be a character vector." %+%
				summate(strs)

			assert(
				is.character(strs), invoking_call,
				message)
		}

	#  -------- collection -------- #

	this$must_be_collection <-
		function (coll, invoking_call) {
			# the value must be a collection.

			coll_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(coll_sym) %+%
				" must be a list, a pairlist or a typed vector." %+%
				summate(coll)

			assert(
				is_collection(coll), invoking_call,
				message)

		}

	this$must_be_recursive <-
		function (coll, invoking_call) {

			coll_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(coll_sym) %+%
				" must be a list or a pairlist." %+%
				summate(coll)

			assert(
				is.recursive(coll), invoking_call,
				message)

		}

	this$must_be_longer_than <-
		function (coll, length, invoking_call) {
			# the collection must be longer than.

			coll_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(coll_sym) %+%
				" must have length longer than " %+% length %+% "." %+%
				summate(coll)

			assert(
				length(coll) >= length, invoking_call,
				message)
		}

	this$must_be_longer_or_equal_than <-
		function (coll, length, invoking_call) {
			# the collection must be longer than.

			coll_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(coll_sym) %+%
				" must have length equal or longer than " %+% length %+%
				"."  %+% summate(coll)

			assert(
				length(coll) >= length, invoking_call,
				message)
		}

		this$must_be_equal_length <-
			function (coll1, coll2, invoking_call) {
				# both collections must have equal lengths.

				coll1_sym <- match.call()[-1][[1]]
				coll2_sym <- match.call()[-1][[2]]

				message <- "both " %+% coll1_sym %+% " and " %+% coll2_sym %+%
					" must have equal lengths."

			}

	#  -------- names  -------- #

	this$must_be_fully_named <-
		function (coll, invoking_call) {
			# the collection should be fully named.

			input_symbol <- match.call()[-1][[1]]

			assert(
				!any(names(coll) == ""),
				invoking_call,
				exclaim$must_be_fully_named(
					input_symbol, summate(colls)) )
		}

	#  -------- collection of collection  -------- #

	this$must_be_collection_of_collections <-
		function (colls, invoking_call) {
			# the value must be a collection of collections.

			colls_sym <- match.call()[-1][[1]]

			message <- "the elements of the collection " %+% dQuote(colls_sym) %+%
				" must all be lists, pairlists or typed vectors." %+%
				summate(colls)

			assert(
				all(sapply(colls, is_collection)),
				invoking_call, message)
		}

	this$must_be_collection_of_fn_matchable <-
		function (fns, invoking_call) {
			# the collection must be composed of
			# lookupables as functions.

			fns_sym <- match.call()[-1][[1]]

			message <- "the arguments matching " %+% dQuote(fns_sym) %+%
				" must all be functions, or symbols or strings" %+%
				" that can be looked-up as functions." %+%
				summate(fns)

			assert(
				all(sapply(fns, is_fn_matchable)), invoking_call,
				message)

		}

	this$must_be_collections_of_length_matching <-
		function (colls, coll, invoking_call) {
			# the collections inside collection has the
			# same length as coll.

			colls_sym <- match.call()[-1][[1]]
			coll_sym <- match.call()[-1][[2]]

			lengths <- paste(lengths, collapse = " or ")

			message <- "the internal collections of the argument matching " %+%
				dQuote(colls_sym) %+% " must have length equal to that of " %+%
				dQuote(coll_sym) %+% "." %+% summate(colls)

			assert(
				all(vapply(colls, length, integer(1)) == length(coll)),
				invoking_call, message)
		}

	this$must_be_collection_of_lengths <-
		function (colls, lengths, invoking_call) {
			# the collection must have values of a certain length.

			coll_sym <- match.call()[-1][[1]]
			lengths <- paste(lengths, collapse = " or ")

			message <- "the argument matching " %+% dQuote(coll_sym) %+%
				" must be a collection of length " %+% lengths %+% " values." %+%
				summate(colls)

		}

	this$must_be_collection_of_equal_length <-
		function (colls, invoking_call) {
			# the collection must be a collection of equal length values.

			colls_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(colls_sym) %+%
				" must be a collection of collections of equal length." %+%
				summate(colls)

			assert(
				length(unique( vapply(colls, length, integer(1)) )) == 1,
				invoking_call, message)

		}

	#  -------- numeric -------- #

	this$must_be_greater_than <-
		function (num, minimum, invoking_call) {
			# the number must be larger than a minimum.

			num_sym <- match.call()[-1][[1]]

			message <- "the number matching " %+% dQuote(num_sym) %+%
				" must be larger than " %+% minimum %+% "." %+%
				summate(num)

			assert(
				num > minimum, invoking_call,
				message)
		}

	this$must_be_grequal_than <-
		function (num, minimum, invoking_call) {
			# the number must be larger or equal than a minimum.

			num_sym <- match.call()[-1][[1]]

			message <- "the number matching " %+% dQuote(num_sym) %+%
				" must be greater or equal to " %+% minimum %+% "." %+%
				summate(num)

			assert(
				num >= minimum, invoking_call,
				message)
		}

	this$minimum_must_be_greater_than <-
		function (nums, minimum, invoking_call) {
			# the minimum number in a vector must be larger than a minimum.

			nums_sym <- match.call()[-1][[1]]

			message <- "the number matching " %+% dQuote(nums_sym) %+%
				" must be larger than " %+% minimum %+% "." %+%
				summate(nums)

			assert(
				min(nums) >= minimum, invoking_call,
				message)
		}

	this$max_must_be_less_than_length_of <-
		function (nums, coll, invoking_call) {
			# the largest value must have length less than a collection.

			nums_sym <- match.call()[-1][[1]]
			coll_sym <- match.call()[-1][[2]]

			message <- "the maximum number in the argument matching " %+% dQuote(nums_sym) %+%
				" must be less than or equal to " %+% " the length of the argument matching" %+%
				dQuote(coll_sym) %+% "." %+%
				summate(nums)

			assert(
				max(nums) <= length(coll), invoking_call,
				message)
		}

	this$must_be_whole <-
		function (nums, invoking_call) {
			# the numbers matching a value must be round.

			nums_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(nums_sym) %+%
				" must be a whole number." %+%
				summate(nums)

			assert(
				all(round(nums) == nums), invoking_call,
				message)
		}

	this$must_be_nonnegative <-
		function (num, invoking_call) {
			# the number must be non-negative.

			num_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% dQuote(num_sym) %+%
				" must be a collection of non-negative numbers." %+%
				summate(num)

			assert(
				all(num > 0), invoking_call,
				message)

		}

	#  -------- binding locked -------- #

	this$must_be_unlocked <-
		function (sym, parent_frame, invoking_call) {
			# the variable cannot be altered.

			message <- "the variable name " %+% dQuote(sym) %+%
				" referenced a locked variable that cannot be altered."

			is_unlocked <- if (exists(sym)) {
				!bindingIsLocked(sym, parent_frame)
			} else {
				True
			}

			assert(
				is_unlocked, invoking_call,
				message)
		}

	this$must_exist <-
		function (sym, parent_frame, invoking_call) {
			# the variable doesn't exist.

			message <- "the variable referenced by the name " %+%
				dQuote(sym) %+% " does not exist."

			assert(
				exists(sym, envir = parent_frame),
				invoking_call, message)
		}

	this

})




