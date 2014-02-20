
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



throw_arrow_error <- function (invoking_call, message) {
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

		local({

			message <- function (param) {
				"the parametre " %+% ddquote(param) %+% " is required but was missing."
			}

			function (param) {

				if (missing(param)) {
					param <- paste(match.call()$param)

					components <- get_call_components(
						invoking_call = sys.call(-1))

					write_error(
						yelp$arrow_function_failed(
							components$invoking, components$calltext, message(param)),
						call. = False)
				}
				True
			}
		})

	#  -------- value -------- #

	this$must_be_a_single_atom <-
		local({

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

			message <- function (pred_sym, pred) {
				"the predicate function " %+% ddquote(pred_sym) %+%
				" produced a non-logical value." %+%
				summate(pred)
			}

			function (result, pred, invoking_call) {
				# predicates must return logical values.

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

			message <- function (fn_sym, fn) {
				"the argument matching " %+% ddquote(fn_sym) %+%
				" must be a non-primitive function." %+%
				summate(fn)
			}

			function (fn, invoking_call) {
				# the function must be non primitive function.

				fn_sym <-match.call()$fn

				if (is.primitive(fn)) {
					throw_arrow_error(
						invoking_call, message(fn_sym, fn))
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

				print(names_sym)

				if ( !all(names %in% xParamsOf(fn)) ) {
					throw_arrow_error(
						invoking_call, message(names_sym, fn_sym, names))
				}

				True
			}
		})

	#  -------- character -------- #

	this$must_be_character <-
		local({

			message <- function (strs_sym, strs) {

				"the argument matching " %+% ddquote(strs_sym) %+%
				" must be a character vector." %+%
				summate(strs)
			}

			function (strs, invoking_call) {
				# the value must be a character vector.

				strs_sym <- match.call()$strs

				if (!is.character(strs)) {
					throw_arrow_error(
						invoking_call, message(strs_sym, strs))
				}

				True
			}
		})

	#  -------- collection -------- #

	this$must_be_collection <-
		local({

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

	this$must_be_recursive <-
		local({

			message <- function (coll_sym, coll) {
				"the argument matching " %+% ddquote(coll_sym) %+%
				" must be a list or a pairlist." %+%
				summate(coll)
			}

			function (coll, invoking_call) {

				coll_sym <- match.call()$coll

				if (!is.recursive(coll)) {
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

			function (coll, length, invoking_call) {
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

			message <- function (coll_sym, coll) {
				"the argument matching " %+% ddquote(colls_sym) %+%
				" must be a collection of collections of equal length." %+%
				summate(colls)
			}

			function (colls, invoking_call) {
				# the collection must be a collection of equal length values.

				colls_sym <- match.call()$colls


				if (!(length(unique( vapply(colls, length, integer(1)) )) == 1)) {
					throw_arrow_error(
						invoking_call, message(coll_sym, coll))
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

			message <- function (nums_sym, coll_sym, nums) {
				"the maximum number in the argument matching " %+% ddquote(nums_sym) %+%
				" must be less than or equal to " %+% " the length of the argument matching" %+%
				ddquote(coll_sym) %+% "." %+%
				summate(nums)
			}

			function (nums, coll, invoking_call) {
				# the largest value must have length less than a collection.

				nums_sym <- match.call()$nums
				coll_sym <- match.call()$coll

				if (max(nums) > length(coll)) {
					throw_arrow_error(
						invoking_call, message(nums_sym, coll_sym, nums))
				}
				True
			}
		})

	this$must_be_whole <-
		local({

			message <- function (nums_sym, nums) {
				"the argument matching " %+% ddquote(nums_sym) %+%
				" must be a whole number." %+%
				summate(nums)
			}

			function (nums, invoking_call) {
				# the numbers matching a value must be round.

				nums_sym <- match.call()$nums

				if (!all(round(nums) == nums)) {
					throw_arrow_error(
						invoking_call, message(nums_sym, nums))
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

				if (!(all(nums) > 0)) {
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

	this$must_be_invoked_with_brackets <-
		local({

			message <- function () {
				"comprehension objects cannot be invoked as a " %+%
				"function: they must be invoked with square brackets ( [] )"
			}

			function (invoking_call) {

				throw_arrow_error(
					invoking_call, message())
			}
			True
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

demand <- local({

	this <- Object()

	this$must_have_yield <-
		local({

			message <- function () {
				"a collection-comprehension must not begin with a " %+%
				"variable bind expression."
			}

			function (indices, invoking_call) {

				assert(
					1 %!in% indices, invoking_call, message())

			}
		})

	this$must_be_unnamed <-
		local({

			message <- function () {
				"a collection-comprehension cannot have named sub-terms."
			}

			function (exprs, invoking_call) {

				assert(
					is.null(names(exprs)), invoking_call,
					message())
			}
		})

	this$must_all_be_matched <-
		function (indices, exprs, invoking_call) {

			exprs_indices <- seq_along(exprs)

			unmatched <- exprs_indices[ exprs_indices %!in% indices ]
			unmatched_str <- paste0(lapply(unmatched, ith_suffix), collapse = ', ')

			message <- "the " %+% unmatched_str %+% " expression " %+%
			"could not be matched as variable bindings, a predicate, or " %+%
			"a yield expression."

			assert(
				length(unmatched) == 0, invoking_call,
				message)
		}

	this$must_have_bindings <-
		local({

			message <- function () {
				"a non-empty collection-comprehension must have " %+%
				"at least one variable binding."
			}

			function (variables, invoking_call) {

				assert(
					length(variables) > 0, invoking_call,
					message())
			}
		})

	this

})
