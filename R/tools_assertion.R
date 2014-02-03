
# To Developers,
#
# assert:
#     Assert checks if a proposition is true, and if it fails
#     throws a helpful error (see say.R).

format_call <- function (call) {
	# call -> string
	# format the call nicely for printing, fixing the representation of ':='.

	if (length(call) == 0) {
		"arrow_function( )"
	} else {

		call <- as.call(lapply(call, function (term) {

			if(is.call(term) && term[[1]] == as.symbol(':=')) {
				eval(term)
			} else {
				term
			}

		}) )

		calltext <- ddparse(call)

		delimiters <- local({
			chars <- strsplit(calltext, '')[[1]]
			chars <- chars[nchar(chars) > 0]

			which(chars %in% c('{'))
		})

		# find an elegant cuttoff if possible.
		cuttof <- if (any(delimiters %in% 35:45)) {
			min(which[delimiters %in% 35:45]) + 1
		} else {
			35
		}

		if (nchar(calltext) > cuttof) {
			paste0(substring(calltext, 1, cuttof), '... [truncated]')
		} else {
			calltext
		}
	}
}

#
# Console colour-checking based on Hadley Wickham's colouring for testthat.
#

write_error <- function (..., call. = True) {
	# to fix wrong terminal type
	# sudo nano ~/.bashrc
	# export TERM=term-color
	# . ~/.bashenv

	message <- c(...)

	this_terminal <- Sys.getenv()["TERM"]
	colour_terminals <- c(
		"screen", "screen-256color",
		"xterm-color", "xterm-256color")

	if (!is.na(this_terminal) && (this_terminal %in% colour_terminals)) {
		stop("\033[0;31m" %+% message %+% "\033[0m", call. = call.)		
	} else {
		stop(message, call.= call.)
	}
}

# --------------------- assertion functions --------------------- #
#

assert <- local({

	consts <- list(margin = 80)

	function (expr, invoking_call, message) {
		# does an expression evaluate to true?
		# if not, throw a lovely error.

		args <- as.list(match.call())[-1]
		this_call <- sys.call()

		if (!is.logical(expr)) {
			write_error(
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

			write_error(
				yelp$arrow_function_failed(
					callname, call, message),
				call. = False)

		}

		True
	}

})

# -------------------------------- insist -------------------------------- #
#
# To Developers,
# insist is a list of functions that provide a minimal interface to an
# assertion. This object exists to reduce the amount of assertion checking
# code needed.
#
#     insist $ must_be_fn_matchable(fn, invoking_call)
#
#

insist <- local({

	this <- Object()

	#  -------- value -------- #

	this$must_be_of_length <-
		function (val, lengths, invoking_call) {
			# the value must have a length in the set of lengths.

			val_sym <- match.call()[-1][[1]]

			lengths_summary <- paste(lengths, collapse = " or ")

			message <- "the argument matching " %+% ddquote(val_sym) %+%
				" must have length " %+% lengths_summary %+% "." %+%
				summate(val)

			assert(
				length(val) %in% lengths, invoking_call,
				message)
		}

	#  -------- functions -------- #

	this$must_be_logical_result <-
		function (result, pred, invoking_call) {
			# predicates must return logical values.

			pred_sym <- match.call()[-1][[2]]

			message <- "the predicate function " %+% ddquote(pred_sym) %+%
				" produced a non-logical value." %+%
				summate(pred)

			assert(
				is.logical(result), invoking_call, message)

		}

	this$must_be_non_primitive <-
		function (fn, invoking_call) {
			# the function must be non primitive function.

			fn_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% ddquote(fn_sym) %+%
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

			message <- "the argument matching " %+% ddquote(fn_sym) %+%
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

			message <- "the argument matching " %+% ddquote(strs_sym) %+%
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

			message <- "the argument matching " %+% ddquote(coll_sym) %+%
				" must be a list, a pairlist or a typed vector." %+%
				summate(coll)

			assert(
				is_collection(coll), invoking_call,
				message)

		}

	this$must_be_recursive <-
		function (coll, invoking_call) {

			coll_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% ddquote(coll_sym) %+%
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

			message <- "the argument matching " %+% ddquote(coll_sym) %+%
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

			message <- "the argument matching " %+% ddquote(coll_sym) %+%
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

			message <- "the names of the collection matching " %+%
			ddquote(input_symbol) %+% " must be a fully named collection."

			assert(
				!is.null(names(coll)) && !any(names(coll) == ""),
				invoking_call, message)
		}

	this$must_be_collection_of_equal_names <-
		function (colls, invoking_call) {

			input_symbol <- match.call()[-1][[1]]

			inner_names <- lapply(colls, names)

			all_empty <- all( vapply(inner_names, is.null, logical(1)) )
			all_equal <- length(unique(inner_names)) == 1

			message <- "the collections in the argument matching " %+%
			ddquote(input_symbol) %+% " must all be unnamed, or all have the " %+%
			"same names."

			assert(
				all_equal || all_empty, invoking_call,
				message)
		}

	#  -------- collection of collection  -------- #

	this$must_be_collection_of_collections <-
		function (colls, invoking_call) {
			# the value must be a collection of collections.

			colls_sym <- match.call()[-1][[1]]

			message <- "the elements of the collection " %+% ddquote(colls_sym) %+%
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

			message <- "the arguments matching " %+% ddquote(fns_sym) %+%
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


			message <- "the internal collections of the argument matching " %+%
				ddquote(colls_sym) %+% " must have length equal to that of " %+%
				ddquote(coll_sym) %+% "." %+% summate(colls)

			assert(
				all(vapply(colls, length, integer(1)) == length(coll)),
				invoking_call, message)
		}

	this$must_be_collection_of_lengths <-
		function (colls, lengths, invoking_call) {
			# the collection must have values of a certain length.

			coll_sym <- match.call()[-1][[1]]
			lengths <- paste(lengths, collapse = " or ")

			message <- "the argument matching " %+% ddquote(coll_sym) %+%
				" must be a collection of length " %+% lengths %+% " values." %+%
				summate(colls)

		}

	this$must_be_collection_of_equal_length <-
		function (colls, invoking_call) {
			# the collection must be a collection of equal length values.

			colls_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% ddquote(colls_sym) %+%
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

			message <- "the number matching " %+% ddquote(num_sym) %+%
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

			message <- "the number matching " %+% ddquote(num_sym) %+%
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

			message <- "the number matching " %+% ddquote(nums_sym) %+%
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

			message <- "the maximum number in the argument matching " %+% ddquote(nums_sym) %+%
				" must be less than or equal to " %+% " the length of the argument matching" %+%
				ddquote(coll_sym) %+% "." %+%
				summate(nums)

			assert(
				max(nums) <= length(coll), invoking_call,
				message)
		}

	this$must_be_whole <-
		function (nums, invoking_call) {
			# the numbers matching a value must be round.

			nums_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% ddquote(nums_sym) %+%
				" must be a whole number." %+%
				summate(nums)

			assert(
				all(round(nums) == nums), invoking_call,
				message)
		}

	this$must_be_nonnegative <-
		function (nums, invoking_call) {
			# the number must be non-negative.

			nums_sym <- match.call()[-1][[1]]

			message <- "the argument matching " %+% ddquote(nums_sym) %+%
				" must be a collection of non-negative numbers." %+%
				summate(nums)

			assert(
				all(nums > 0), invoking_call,
				message)

		}

	#  -------- binding locked -------- #

	this$must_be_unlocked <-
		function (sym, parent_frame, invoking_call) {
			# the variable cannot be altered.

			message <- "the variable name " %+% ddquote(sym) %+%
				" referenced a locked variable that cannot be altered."

			is_unlocked <- if (exists(sym, where = parent_frame)) {
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

			sym <- toString(sym)

			message <- "the variable referenced by the name " %+%
				ddquote(sym) %+% " does not exist."

			assert(
				exists(sym, envir = parent_frame),
				invoking_call, message)
		}

	this

})




