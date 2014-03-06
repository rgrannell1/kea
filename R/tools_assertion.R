#
#' @section stringify_call:
#'
#' stringify_call is a tool for formatting the source call of an error message.
#' It fixes as issue with printing ':=' functions, and cuts off very long calls.
#'
#' @keywords internal
#' @rdname pkg-internal
#'

stringify_call <- function (call) {
	# call -> string
	# format the call nicely for printing, fixing the representation of ':='.

	if (length(call) == 0) {
		"[ call information not included in error ]"
	} else {

		call <- as.call(lapply(call, function (term) {

			if(is.call(term) && term[[1]] == as.symbol(':=')) {
				eval(term)
			} else {
				term
			}

		}) )

		calltext <- ddparse(call)

		if (nchar(calltext) > 35) {
			paste0(substring(calltext, 1, 35), ' [truncated]', collapse = '')
		} else {
			calltext
		}
	}
}

#' @section colourise:
#'
#' colourise is a set of functions that wraps strings in ansii escape sequences
#' on coloured terminals, allowing coloured text to be printed. These are
#' used to colour error messages and print methods.
#'
#' @keywords internal
#' @rdname pkg-internal
#'

colourise <- local({

	supports_colour <- function () {
		# is a terminal colourisable.

		terminal <- Sys.getenv()["TERM"]
		colour_terminals <- c(
			"screen", "screen-256color", "xterm-color", "xterm-256color")

		!is.na(terminal) && (terminal %in% colour_terminals)
	}

	list(
		black =
			function (message) {
				if (supports_colour()) {
					"\033[0;30m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		red =
			function (message) {
				if (supports_colour()) {
					"\033[0;31m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		green =
			function (message) {
				if (supports_colour()) {
					"\033[0;32m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		blue =
			function (message) {
				if (supports_colour()) {
					"\033[0;34m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		yellow =
			function (message) {
				if (supports_colour()) {
					"\033[1;33m" %+% message %+% "\033[0m"
				} else {
					message
				}
			}
	)
})

#' @section write_error:
#'
#' write_error prints a red error message to the terminal,
#' if the terminal supports colour.
#'
#' @keywords internal
#' @rdname pkg-internal
#'

write_error <- function (..., call. = True) {
	# to fix wrong terminal type
	# sudo nano ~/.bashrc
	# export TERM=term-color
	# . ~/.bashenv

	message <- c(...)

	stop(colourise$red(message), call. = call.)

}

#' @section assert:
#'
#' assert is a key arrow function - it takes an expression,
#' a call to display, and a string message. If the expression isn't
#' true, the error is thrown with that message and call.
#'
#' @keywords internal
#' @rdname pkg-internal
#'

get_call_components <- function (invoking_call) {
	# get the calling function and call text from a call.
	list(
		invoking =
			paste0(invoking_call[[1]], collapse = ''),
		calltext =
			stringify_call(invoking_call))
}

assert <- local({

	function (expr, invoking_call, message) {
		# does an expression evaluate to true?
		# if not, throw a lovely error.

		if (!is.logical(expr)) {
			# the assertion was broken.

			write_error(
				yelp$non_logical_assertion(expr),
				call. = False)

		} else if (!isTRUE(expr)) {
			# everythings went wrong, throw an error.

			components <- get_call_components(invoking_call)

			write_error(
				yelp$arrow_function_failed(
					components$invoking, components$calltext, message),
				call. = False)
		}
		True
	}
})



throw_arrow_error <- throw <- function (invoking_call, message) {
	# everythings went wrong, throw an error.

	components <- get_call_components(invoking_call)

	write_error(
		yelp$arrow_function_failed(
			components$invoking, components$calltext, message),
			call. = False)
}



#' @section insist:
#'
#' insist is a list of functions that provide a minimal interface to an
#' assertion. This object exists to reduce the amount of assertion checking
#' code needed.
#'
#'     \code{insist $ must_be_fn_matchable(fn, invoking_call)}
#'
#' Each function encloses a message function to stop the function being
#' repeatedly created when assertions are ran. When the assertion inside
#' the insist function fails, an error is thrown.
#'
#' @keywords internal
#' @rdname pkg-internal
#'


insist <- local({

	this <- Object()

	this$must_not_be_missing <-
		# test if a function parametre is missing, or if the
		# parametre evaluates to an undefined value.

		local({

			message <- function (param) {
				"the parametre " %+% ddquote(param) %+% " is required but was missing."
			}

			function (param) {

				evaluating_throws_error <- tryCatch({
						#eval(param, parent.frame())
						False
					},
					warning =
						function (warn) {
							True
						},
					error =
						function (err) {
							True
						}
				)

				if (missing(param) || evaluating_throws_error) {

					param <- toString(match.call()$param)

					components <- get_call_components(sys.call(-1))

					write_error(
						yelp$arrow_function_failed(
							components$invoking, components$calltext, message(param)),
						call. = False)
				}
				True
			}
		})

	this$must_not_be_missing_sym <-
		# test if a function parametre is missing, but not whether
		# the value evalutates.

		local({

			message <- function (param) {
				"the parametre " %+% ddquote(param) %+% " is required but was missing."
			}

			function (param) {

				if (missing(param)) {
					param <- paste(match.call()$param)

					components <- get_call_components(sys.call(-1))

					write_error(
						yelp$arrow_function_failed(
							components$invoking, components$calltext, message(param)),
						call. = False)
				}
				True
			}
		})

	#  -------- value -------- #

	this$must_be_atom <-
		local({
			# test if a value is a convertable to an atomic vector of length one.

			message <- function (val_sym, val) {

				"the argument matching must be a length-one or zero " %+%
				"atomic value" %+% "." %+% summate(val)
			}

			function (val, invoking_call) {

				val_sym <- match.call()$val

				not_vector <- !(is.atomic(val) || is.list(val))

				too_long <- length(val) > 1

				first_not_atom <- if (length(val) > 0) {
					length( val[[1]] ) != 1
				} else {
					False
				}

				if (not_vector || too_long || first_not_atom) {

					throw_arrow_error(
						invoking_call, message(val_sym, val))
				}
				True
			}
		})

	this$must_be_of_length <-
		local({
			# test if a value has a certain length.

			message <- function (val_sym, lengths, val) {

				lengths_summary <- paste(lengths, collapse = " or ")

				"the argument matching " %+% ddquote(val_sym) %+%
				" must have length " %+% lengths_summary %+% "." %+%
				summate(val)
			}

			function (val, lengths, invoking_call) {
				# the value must have a length in the set of lengths.

				val_sym <- match.call()$val

				if (length(val) %!in% lengths) {
					throw_arrow_error(
						invoking_call, message(val_sym, lengths, val))
				}

				True
			}
		})

	#  -------- functions -------- #

	this$must_be_logical_result <-
		local({
			# test if a value is true, false or na.

			message <- function (pred_sym, pred) {
				"the predicate function " %+% ddquote(pred_sym) %+%
				" produced a non-logical value." %+%
				summate(pred)
			}

			function (result, pred, invoking_call) {

				pred_sym <- match.call()$pred

				if (!is.logical(result)) {
					throw_arrow_error(
						invoking_call, message(pred_sym, pred))
				}

				True
			}
		})

	this$must_be_non_primitive <-
		local({
			# test if a function is non-primative.

			message <- function (fn_sym, fn) {
				"the argument matching " %+% ddquote(fn_sym) %+%
				" must be a non-primitive function." %+%
				summate(fn)
			}

			function (fn, invoking_call) {

				fn_sym <-match.call()$fn

				if (is.primitive(fn)) {
					throw_arrow_error(
						invoking_call, message(fn_sym, fn))
				}

				True
			}
		})

	this$must_be_matchable <-
		local({

			message <- function (val_sym, val) {
				"the argument matching " %+% ddquote(val_sym) %+%
				" must be a symbol or string. "%+%
				summate(val)
			}

			function (val, invoking_call) {

				val_sym <- match.call()$val

				if (!is.name(val) && (!is.character(val) || length(val) != 1)) {
					throw_arrow_error(
						invoking_call, message(val_sym, val))
				}

				True
			}
		})

	this$must_be_fn_matchable <-
		local({

			message <- function (fn_sym, fn) {
				"the argument matching " %+% ddquote(fn_sym) %+%
				" must be a function, or a symbol or string" %+%
				" that can be looked-up as a function. "%+%
				summate(fn)
			}

			function (fn, invoking_call) {
				# the value must be lookupable as a function.

				fn_sym <- match.call()$fn

				if (!is_fn_matchable(fn)) {
					throw_arrow_error(
						invoking_call, message(fn_sym, fn))
				}

				True
			}
		})

	this$must_be_parametres_of <-
		local({

			message <- function (names_sym, fn_sym, names) {

				names <- names[nchar(names) > 0]

				"the elements of " %+% names_sym %+%
				" must be parametre names of " %+% fn_sym %+% "." %+%
				summate(names)
			}

			function (names, fn, invoking_call) {
				# the names given must be parametre names
				# of the function given.

				names_sym <- match.call()$names
				fn_sym <- match.call()$fn

				if ( !all(names %in% xParamsOf(fn)) ) {
					throw_arrow_error(
						invoking_call, message(names_sym, fn_sym, names))
				}

				True
			}
		})

	#  -------- character -------- #

	#  -------- collection -------- #

	this$must_be_collection <-
		local({
			# test if a value is a list, pairlist or atomic vector.

			message <- function (coll_sym, coll) {
				"the argument matching " %+% ddquote(coll_sym) %+%
				" must be a list, a pairlist or a typed vector." %+%
				summate(coll)
			}

			function (coll, invoking_call) {
				# the value must be a collection.

				coll_sym <- match.call()$coll

				if (!is_collection(coll)) {
					throw_arrow_error(
						invoking_call, message(coll_sym, coll))
				}

				True
			}
		})

	this$must_be_longer_than <-
		local({

			message <- function (coll_sym, len, coll) {
				"the argument matching " %+% ddquote(coll_sym) %+%
				" must have length longer than " %+% length %+% "." %+%
				summate(coll)
			}

			function (coll, len, invoking_call) {
				# the collection must be longer than.

				coll_sym <- match.call()$coll

				if (len > length(coll)) {
					throw_arrow_error(
						invoking_call, message(coll_sym, len, coll))
				}

				True
			}
		})

	this$must_be_longer_or_equal_than <-
		local({

			message <- function (coll_sym, length, coll) {
				"the argument matching " %+% ddquote(coll_sym) %+%
				" must have length equal or longer than " %+% length %+%
				"."  %+% summate(coll)
			}

			function (coll, length, invoking_call) {
				# the collection must be longer than.

				coll_sym <- match.call()$coll

				if (!(length(coll) >= length)) {
					throw_arrow_error(
						invoking_call, message(coll_sym, length, coll))
				}

				True
			}
		})

		this$must_be_equal_length <-
			local({

				message <- function (coll1_sym, coll2_sym) {
					"both " %+% coll1_sym %+% " and " %+% coll2_sym %+%
					" must have equal lengths."
				}

				function (coll1, coll2, invoking_call) {
					# both collections must have equal lengths.

					coll1_sym <- match.call()$coll1
					coll2_sym <- match.call()$coll2

					if (length(coll1) != length(coll2)) {
						throw_arrow_error(
							invoking_call, message(coll1_sym, coll2_sym))
					}

					True
				}
			})

	#  -------- names  -------- #

	this$must_be_fully_named <-
		local({

			message <- function (input_symbol) {
				"the names of the collection matching " %+%
				ddquote(input_symbol) %+% " must be a fully named collection."
			}

			function (coll, invoking_call) {
				# the collection should be fully named.

				coll_symbol <- match.call()$coll

				if (is.null(names(coll)) || any(names(coll) == "")) {
					throw_arrow_error(
						invoking_call, message(coll_symbol))
				}

				True
			}
		})

	this$must_be_collection_of_equal_names <-
		local({

			message <- function (input_symbol) {
				"the collections in the argument matching " %+%
				ddquote(input_symbol) %+% " must all be unnamed, or all have the " %+%
				"same names."
			}

			function (colls, invoking_call) {

				colls_sym <- match.call()$colls

				inner_names <- lapply(colls, names)

				all_empty <- all( vapply(inner_names, is.null, logical(1)) )
				all_equal <- length(unique(inner_names)) == 1

				if (!all_equal && !all_equal) {
					throw_arrow_error(
						invoking_call, message(colls_sym))
				}

				True
			}
		})

	#  -------- collection of collection  -------- #

	this$must_be_collection_of_collections <-
		local({

			message <- function (colls_sym, colls) {
				"the elements of the collection " %+% ddquote(colls_sym) %+%
				" must all be lists, pairlists or typed vectors." %+%
				summate(colls)
			}

			function (colls, invoking_call) {
				# the value must be a collection of collections.

				colls_sym <- match.call()$colls

				if (!all(sapply(colls, is_collection))) {

					throw_arrow_error(
						invoking_call, message(colls_sym, colls))
				}

				True
			}
		})

	this$must_be_collection_of_fn_matchable <-
		local({

			message <- function (fns_sym, fns) {
				"the arguments matching " %+% ddquote(fns_sym) %+%
				" must all be functions, or symbols or strings" %+%
				" that can be looked-up as functions." %+%
				summate(fns)
			}

			function (fns, invoking_call) {
				# the collection must be composed of lookupables as functions.

				fns_sym <- match.call()$fns

				if (!all(sapply(fns, is_fn_matchable))) {

					throw_arrow_error(
						invoking_call, message(fns_sym, fns))
				}

				True

			}
		})

	this$must_be_collections_of_length_matching <-
		local({

			message <- function (colls_sym, coll_sym, colls) {
				"the internal collections of the argument matching " %+%
				ddquote(colls_sym) %+% " must have length equal to that of " %+%
				ddquote(coll_sym) %+% "." %+% summate(colls)
			}

			function (colls, coll, invoking_call) {
				# the collections inside collection has the
				# same length as coll.

				colls_sym <- match.call()$colls
				coll_sym <- match.call()$coll

				if ( !all(vapply(colls, length, integer(1)) == length(coll)) ) {
					throw_arrow_error(
						invoking_call, message(colls_sym, coll_sym, colls))
				}

				True
			}
		})

	this$must_be_collections_of_length_grequal_than <-
		local({

			message <- function (colls_sym, num_sym, colls) {
				"the internal collections of the argument matching " %+%
				ddquote(colls_sym) %+% " must have length greater than or equal to " %+%
				ddquote(num_sym) %+% "." %+% summate(colls)
			}

			function (colls, num, invoking_call) {
				# the collections inside collection has the
				# same length as coll.

				colls_sym <- match.call()$colls
				num_sym <- match.call()$num

				if ( !all(vapply(colls, length, integer(1)) >= num) ) {
					throw_arrow_error(
						invoking_call, message(colls_sym, num_sym, colls))
				}

				True
			}
		})

	this$must_be_collection_of_lengths <-
		local({

			message <- function (colls_sym, lengths, colls) {

				lengths <- paste(lengths, collapse = " or ")

				"the argument matching " %+% ddquote(colls_sym) %+%
				" must be a collection of length " %+% lengths %+% " values." %+%
				summate(colls)
			}

			function (colls, lengths, invoking_call) {
				# the collection must have values of a certain length.

				colls_sym <- match.call()$colls

				if (!all(vapply(colls, length, integer(1)) %in% lengths)) {

					throw_arrow_error(
						invoking_call, message(colls_sym, lengths, colls))

				}

				True
			}
		})

	this$must_be_collection_of_equal_length <-
		local({

			message <- function (colls_sym, colls) {
				"the argument matching " %+% ddquote(colls_sym) %+%
				" must be a collection of collections of equal length." %+%
				summate(colls)
			}

			function (colls, invoking_call) {
				# the collection must be a collection of equal length values.

				colls_sym <- match.call()$colls

				if (!length(colls) == 0 &&
					!(length(unique( vapply(colls, length, integer(1)) )) == 1)) {
					throw_arrow_error(
						invoking_call, message(colls_sym, colls))
				}

				True
			}
		})
	#  -------- numeric -------- #

	this$must_be_greater_than <-
		local({

			message <- function (num_sym, minimum, num) {
				"the number matching " %+% ddquote(num_sym) %+%
				" must be larger than " %+% minimum %+% "." %+%
				summate(num)
			}

			function (num, minimum, invoking_call) {
				# the number must be larger than a minimum.

				num_sym <- match.call()$num

				if (!(num > minimum)) {
					throw_arrow_error(
						invoking_call, message(num_sym, minimum, num))
				}

				True
			}
		})

	this$must_be_grequal_than <-
		local({

			message <- function (num_sym, minimum, num) {
				"the number matching " %+% ddquote(num_sym) %+%
				" must be greater or equal to " %+% minimum %+% "." %+%
				summate(num)
			}

			function (num, minimum, invoking_call) {
				# the number must be larger or equal than a minimum.

				num_sym <- match.call()$num

				if (!(num >= minimum)) {
					throw_arrow_error(
						invoking_call, message(num_sym, minimum, num))
				}

				True
			}
		})

	this$minimum_must_be_greater_than <-
		local({

			message <- function (nums_sym, minimum, nums) {
				"the number matching " %+% ddquote(nums_sym) %+%
				" must be larger than " %+% minimum %+% "." %+%
				summate(nums)
			}

			function (nums, minimum, invoking_call) {
				# the minimum number in a vector must be larger than a minimum.

				nums_sym <- match.call()$nums

				if (!(min(nums) >= minimum)) {
					throw_arrow_error(
						invoking_call, message(nums_sym, minimum, nums))
				}

				True
			}
		})

	this$max_must_be_less_than_length_of <-
		local({
			# the largest number in a collection must be lequal than the length of
			# the collection

			message <- function (nums_sym, coll_sym, nums) {
				"the maximum number in the argument matching " %+% ddquote(nums_sym) %+%
				" must be less than or equal to " %+% " the length of the argument matching" %+%
				ddquote(coll_sym) %+% "." %+%
				summate(nums)
			}

			function (nums, coll, invoking_call) {

				nums_sym <- match.call()$nums
				coll_sym <- match.call()$coll

				if (max(nums) > length(coll)) {
					throw_arrow_error(
						invoking_call, message(nums_sym, coll_sym, nums))
				}
				True
			}
		})

	this$must_be_positive_indices_of <-
		local({
			# test that nums are indices of a collection
			# not including negative values.

			message <- function (nums_sym, coll_sym, nums) {
				"the argument matching " %+% ddquote(nums_sym) %+%
				" must be positive indices of " %+% ddquote(coll_sym) %+% "." %+%
				summate(nums)
			}

			function (nums, coll, invoking_call) {

				nums_sym <- match.call()$nums
				coll_sym <- match.call()$coll

				if (max(nums) > length(coll) || min(nums) < 1) {
					throw_arrow_error(
						invoking_call, message(nums_sym, coll_sym, nums))
				}

				True
			}
		})

	this$must_be_indices_of <-
		local({
			# test that nums are indices of a collection, including negative values.

			message <- function (nums_sym, coll_sym, nums) {
				"the argument matching " %+% ddquote(nums_sym) %+%
				" must be positive or negative indices of " %+% ddquote(coll_sym) %+% "." %+%
				summate(nums)
			}

			function (nums, coll, invoking_call) {

				nums_sym <- match.call()$nums
				coll_sym <- match.call()$coll

				if (max(nums) > length(coll) || min(nums) < -length(coll)) {
					throw_arrow_error(
						invoking_call, message(nums_sym, coll_sym, nums))
				}

				True
			}
		})

	this$must_be_nonnegative <-
		local({

			message <- function (nums_sym, nums) {
				"the argument matching " %+% ddquote(nums_sym) %+%
				" must be a collection of non-negative numbers." %+%
				summate(nums)
			}

			function (nums, invoking_call) {
				# the number must be non-negative.

				nums_sym <- match.call()$nums

				if ( !(all(nums >= 0)) ) {
					throw_arrow_error(
						invoking_call, message(nums_sym, nums))
				}
				True
			}
		})

	#  -------- binding locked -------- #

	this$must_be_unlocked <-
		local({

			message <- function (sym) {
				"the variable name " %+% ddquote(sym) %+%
				" referenced a locked variable that cannot be altered."
			}

			function (sym, parent_frame, invoking_call) {
				# the variable cannot be altered.

				is_unlocked <- if (exists(sym, envir = parent_frame)) {
					!bindingIsLocked(sym, env = parent_frame)
				} else {
					True
				}

				if (!is_unlocked) {
					throw_arrow_error(
						invoking_call, message(sym))
				}
				True
			}
		})

	this$must_exist <-
		local({

			message <- function (sym) {
				"the variable referenced by the name " %+%
				ddquote(sym) %+% " does not exist."
			}

			function (sym, parent_frame, invoking_call) {
				# the variable doesn't exist.

				sym <- toString(sym)

				if (!exists(sym, envir = parent_frame)) {
					throw_arrow_error(
						invoking_call, message(sym))
				}
				True
			}
		})

	this$must_be_correct_type <-
		local({

			message <- function (coll_sym, coll, mode) {
				"the collection " %+% ddquote(coll_sym) %+% " cannot be " %+%
				"unlisted to a vector of mode " %+% ddquote(mode) %+%
				summate(coll)
			}

			function (coll_sym, coll, mode, invoking_call) {

				# numeric is a superset of integer and double

				type <- typeof(coll)

				if (mode == 'numeric') {

					if ( all(type != c('integer', 'double', 'numeric')) ) {
						throw_arrow_error(
							invoking_call, message(coll_sym, coll, mode))
					}

				# other types should be the same
				} else if (!type == mode) {

					throw_arrow_error(
						invoking_call, message(coll_sym, coll, mode))
				}
				True
			}
		})

	this$must_be_unlistable <-
		local({
			# a vector must be convertable to a type.

			message_type <- function (coll_sym, mode) {
				"the collection " %+% ddquote(coll_sym) %+%
				" must be a collection of values of type " %+% ddquote(mode)
			}

			#message_length()

			is_valid_elem <- function (elem, mode) {

				if (length(elem) != 1) {
					stop("")
				} else if (!is(elem, mode)) {
					stop("")
				}
				TRUE
			}

			function (coll_sym, coll, mode, invoking_call) {

				vapply(coll, is_valid_elem, logical(1), mode = mode)
				True
			}
		})

	this$must_be_existing_file <-
		local({

			message <- function (str, invoking_call) {
				"the file " %+% dQuote(str) %+% " does not exist."
			}

			function (str, invoking_call) {

				if (!file.exists(str)) {
					throw_arrow_error(
						invoking_call, message(str, invoking_call))
				}

				True
			}
		})

	this

})

#' @section demand:
#'
#' An internal object containing assertions that are required by the
#' collection comprehension functions. Mostly contains parse error
#' assertions.
#'
#' @keywords internal
#'
#' @rdname pkg-internal

# -------------------------------- dictate -------------------------------- #
#
# To Developers,
#
# dictate contains assertions specific to xLambda.

dictate <- local({

	this <- Object()

	this$must_have_correct_delimiter <-
		local({

			message <- function (actual, expected) {
				"the unexpected delimiter " %+% dQuote(actual) %+%
				" was encountered, but the delimiter " %+% dQuote(expected) %+%
				" was expected."
			}

			function (get_tree, token, tree) {

				if (get_tree$delim(tree) != token$delim()) {

					throw_arrow_error(
						invoking_call, message( get_tree$delim(tree), token$delim() ))

				}
				True
			}

		})

	this$must_have_symbol_params <-
		function () {

		}

	this$must_have_enclosing_params <-
		function () {

		}

	this
})





























































Must <- local({

	this <- Object()

	this $ Be_Between <-
		function (NUMS, LOWER, UPPER) {

			NUMS <- match.call()$NUMS

			bquote(if (any( .(NUMS) > .(UPPER) | .(NUMS) < .(LOWER) )) {

				message <-
					"the argument matching " %+% ddquote( .(NUMS) ) %+%
					" must be in the range {" %+% .(LOWER) %+% "..." %+% .(UPPER) %+% "}." %+%
					summate( .(NUMS) )

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Collection <-
		function (COLL) {
			# this macro expands to check if a value is a collection.

			COLL <- match.call()$COLL

			bquote(if (!is.atomic( .(COLL) ) && !is.list( .(COLL) ) && !is.pairlist( .(COLL) )) {

				message <-
					"the argument matching " %+% ddquote( .(COLL) ) %+%
					" must be a list, a pairlist or a typed vector." %+%
					summate( .(COLL) )

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Collection_Of_Collections <-
		function (COLLS) {

			COLLS <- match.call()$COLLS

			bquote({

				all_elems_are_collection <- all( vapply( .(COLLS) , function (coll) {

					is.atomic(coll) || is.list(coll) || is.pairlist(coll)

				}, logical(1)) )

				if (!all_elems_are_collection) {

					message <-
						"the argument matching " %+% ddquote( .(COLLS) ) %+%
						" must be a collection of lists, vectors or pairlists." %+%
						summate( .(COLLS) )

					throw_arrow_error(invoking_call, message)
				}

			})
		}

	this $ Be_Collection_Of_Equal_Length <-
		function (COLLS) {

			COLLS <- match.call()$COLLS

			bquote({

				all_equal <-
					length( .(COLLS) ) == 0 ||
					all(vapply( .(COLLS), function (coll) length(coll) == length( .(COLLS)[[1]] ), logical(1)))

				if (!all_equal) {

					message <-
						"the argument matching " %+% ddquote( .(COLLS) ) %+%
						" must be a collection of collections with equal lengths." %+%
						summate( .(COLLS) )

					throw_arrow_error(invoking_call, message)

				}
			})

		}

	this $ Be_Collection_Of_Fn_Matchable <-
		function (COLL) {

			COLL <- match.call()$COLL

			bquote({

				all_match <- all( vapply( .(COLL) , function (val) {

					is.function(val) ||
					(is.character(val) && length(val) == 1) ||
					is.name(val)

				}, logical(1)) )

				if (!all_match) {

					message <-
						"the argument matching " %+% ddquote( .(COLL) ) %+%
						" must be a collection of functions, or symbols or strings" %+%
						" that can be looked up as functions."

					throw_arrow_error(invoking_call, message)
				}

			})
		}

	this $ Be_Collection_Of_Lengths_In_Range <-
		function (COLLS, LOWER, UPPER) {

			COLLS <- match.call()$COLLS
			LOWER <- match.call()$LOWER
			UPPER <- match.call()$UPPER

			bquote( if (any(vapply( .(COLLS), function (coll) {
				length(coll) >= .(LOWER) && length(coll) <= .(UPPER)
			}, logical(1) )) ) {

				message <-
					"the argument matching " %+% ddquote( .(COLLS) ) %+%
					" must be a collection with lengths in the range " %+%
					.(LOWER) %+% " to " %+% .(UPPER) %+% "."

			})
		}

	this $ Be_Equal_Length_To <-
		function (COLL1, COLL2) {

			COLL1 <- match.call()$COLL1
			COLL2 <- match.call()$COLL2

			bquote(if (length( .(COLL1) ) != length( .(COLL2) )) {

				message <-
					"the argument matching " %+% ddquote( .(COLL1) ) %+%
					" must be equal length to the argument matching " %+% ddquote( .(COLL2) ) %+% "." %+%
					summate( .(COLL1) )

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Existing_Ref <-
		function (SYM) {

			SYM <- match.call()$SYM

			bquote(if ( !exists( .(SYM), envir = parent.frame()) ) {

				message <-
					"the variable referenced by the symbol " %+% ddquote( .(SYM) ) %+%
					" does not exist."

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Flag <-
		function (BOOL, PRED) {
			# this macro expands to check if a value is True, False or Na.

			BOOL <- match.call()$BOOL
			PRED <- match.call()$PRED

			bquote(if (!is.logical( .(BOOL) ) || length( .(BOOL) ) != 1) {

				message <-
					"the predicate function " %+% ddquote( .(PRED) ) %+%
					" produced a non-{True, False, Na} value." %+%
					summate( .(BOOL) )

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_File <-
		function (STR) {

			STR <- match.call()$STR

			bquote(if (!file.exists( .(STR) )) {

				message <-
					"the argument matching " %+% ddquote( .(STR) ) %+%
					" must be a path to an existing file."

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Fn_Matchable <-
		function (VAL) {
			# this macro expands to check if a value is a function or
			# can be looked up as a function.

			VAL <- match.call()$VAL

			bquote(if (
				!is.function( .(VAL) ) &&
				!is.name( .(VAL) ) &&
				!(is.character( .(VAL) ) && length( .(VAL) ) == 1)) {

					message <-
						"the argument matching " %+% ddquote( .(VAL) ) %+%
						" must be a function, or a string or symbol naming a function." %+%
						summate( .(VAL) )

					throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Indices <-
		function (NUMS, COLL) {

			NUMS <- match.call()$NUMS
			COLL <- match.call()$COLL

			bquote(if (any( .(NUMS) > length( .(COLL) ) | .(NUMS) < -length( .(COLL) ) )) {

				message <-
					"the argument matching " %+% ddquote( .(NUMS) ) %+%
					" must be positive indices of the collection matching " %+% ddquote( .(COLL) ) %+% "." %+%
					summate( .(NUMS) )

			})
		}

	this $ Be_Of_Length <-
		function (COLL, LENGTHS) {
			# this macro expands to check that a collection has a certain length.

			COLL <- match.call()$COLL
			LENGTHS <- match.call()$LENGTHS

			bquote(if (length( .(COLL) ) %!in% .(LENGTHS)) {

				message <-
					"the argument matching " %+% ddquote( .(COLL) ) %+%
					" must have length" %+% paste( .(LENGTHS, collapse = ' or ') ) %+% "." %+%
					summate( .(COLL) )

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Lequal_Than <-
		function (COLL, LENGTH) {
			# this macro expands to check that a collection is lequal than a certain length.

			COLL <- match.call()$COLL
			LENGTH <- match.call()$LENGTH

			bquote(if (!(length( .(COLL) ) >= .(LENGTH) )) {

				message <-
					"the argument matching " %+% ddquote( .(COLL) ) %+%
					" must have at least " %+%  .(LENGTH) %+% " elements." %+%
					summate( .(COLL) )

				throw_arrow_error(invoking_call, message)
			})

		}

	this $ Be_Longer_Than <-
		function (LENGTH, COLL) {
			# this macro expands to check that a collection is longer than a certain length.

			COLL <- match.call()$COLL
			LENGTH <- match.call()$LENGTH

			bquote(if (!(length( .(COLL) ) > .(LENGTH) )) {

				message <-
					"the argument matching " %+% ddquote( .(COLL) ) %+%
					" must have more than " %+%  .(LENGTH) %+% " elements." %+%
					summate( .(COLL) )

				throw_arrow_error(invoking_call, message)
			})

		}

	this $ Be_Matchable <-
		function (SYM) {
			# this macro expands to test if a value is a symbol.

			SYM <- match.call()$SYM

			bquote({

				if (!is.name( .(SYM) ) && (!is.character( .(SYM) ) || length( .(SYM) ) != 1)) {

					message <-
						"the argument matching " %+% ddquote( .(SYM) ) %+%
						" must be a symbol or a string." %+%
						summate( .(SYM) )

					throw_arrow_error(invoking_call, message)
				}
			})


		}

	this $ Be_Named <-
		function (COLL) {

			COLL <- match.call()$COLL

			bquote({

				if ( is.null(names( .(COLL) )) ) {

					message <-
						"the argument matching " %+% ddquote( .(COLL) ) %+%
						" must be named." %+%
						summate( .(COLL) )

					throw_arrow_error(invoking_call, message)
				}
			})
		}

	this $ Be_Parametres_Of <-
		function (STRS, FN) {
			# this macro expands to check if a set of names are parametres of a function.

			STRS <- match.call()$STRS
			FN <- match.call()$FN

			bquote(if (any( .(STRS) %!in% names(formals( .(FN) )) )) {

				message <-
					"the argument matching " %+% ddquote( .(STRS) ) %+%
					" must be parametres of the function matching " %+% ddquote( .(FN) ) %+% "." %+%
					summate( .(STRS) )

				throw_arrow_error(invoking_call, message)
			})
		}

	this $ Be_Positive_Indices <-
		function (NUMS, COLL) {

			NUMS <- match.call()$NUMS
			COLL <- match.call()$COLL

			bquote(if (any( .(NUMS) > length( .(COLL) ) | .(NUMS) < 1 )) {

				message <-
					"the argument matching " %+% ddquote( .(NUMS) ) %+%
					" must be positive indices of the collection matching " %+% ddquote( .(COLL) ) %+% "." %+%
					summate( .(NUMS) )

			})
		}

	this $ Be_Whole <-
		function (NUMS) {

			NUMS <- match.call()$NUMS

			bquote(if (!all(round( .(NUMS) ) ==  .(NUMS) )) {

				message <-
					"the argument matching " %+% ddquote( .(NUMS) ) %+%
					" must be round numbers." %+%
					summate( .(NUMS) )

			})
		}

	this $ Not_Be_Missing <-
		function (VAL) {
			# this macro expands to check if a parametre is not missing.

			VAL <- match.call()$VAL

			bquote(if (missing( .(VAL) )) {

				message <-
					"the parametre " %+% ddquote( .(VAL) ) %+%
					" is required but was missing."

				throw_arrow_error(invoking_call, message)
			})

		}

	this $ Not_Be_Primitive <-
		function (FN) {
			# this macro expands to check if a function is non-primitive.

			FN <- match.call()$FN

			bquote(if (is.primitive( .(FN) )) {

				message <-
					"the argument matching " %+% ddquote( .(FN) ) %+%
					" must be a non-primitive function." %+%
					summate( .(FN) )

				throw_arrow_error(invoking_call, message)
			})
		}




	this
})





ddquote <- function (SYM) {

	SYM <- match.call()$SYM

	paste0(dQuote(SYM), collapse = '')
}

MakeFun <- function (expr) {

	unquote <- function (inner) {

		if (is.pairlist(inner)) {
			as.pairlist(lapply(inner, unquote))
		} else if (length(inner) <= 1L) {
			inner
		} else if (inner[[1L]] == as.name("MACRO")) {
			eval(inner[[2L]], parent.frame())
		} else {
			as.call(lapply(inner, unquote))
		}
	}

	eval(unquote(substitute(expr)), parent.frame())
}
