
# To Developers,
#
# assert:
#     Assert checks if a proposition is true, and if it fails
#     throws a helpful error (see say.R).

format_call <- function (call) {
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

		# find an elegant cuttoff if possible.
		cuttof <- 35

		if (nchar(calltext) > cuttof) {
			paste0(substring(calltext, 1, cuttof), ' [truncated]')
		} else {
			calltext
		}
	}
}

# -------------------------------- colourise -------------------------------- #
#
# To Developers,
# colourise is a set of functions that wraps strings in ansii escape sequences
# on coloured terminals, allowing coloured text to be printed.

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


write_error <- local({

	function (..., call. = True) {
		# to fix wrong terminal type
		# sudo nano ~/.bashrc
		# export TERM=term-color
		# . ~/.bashenv

		message <- c(...)

		stop(colourise$red(message), call. = call.)

	}
})


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
# Each function encloses a message function to stop the function being
# repeatedly created when assertions are ran.

insist <- local({

	this <- Object()

	#  -------- value -------- #

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

				val_sym <- match.call()[-1][[1]]

				assert(
					length(val) %in% lengths, invoking_call,
					message(val_sym, lengths, val))
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

				pred_sym <- match.call()[-1][[2]]

				assert(
					is.logical(result), invoking_call, message(pred_sym, pred))

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

				fn_sym <- match.call()[-1][[1]]

				assert(
					!is.primitive(fn), invoking_call,
					message(fn_sym, fn))
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

				fn_sym <- match.call()[-1][[1]]

				assert(
					is_fn_matchable(fn),
					invoking_call, message(fn_sym, fn))
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

				names_sym <- match.call()[-1][[1]]
				fn_sym <- match.call()[-1][[2]]

				assert(
					all(names %in% xParamsOf(fn)), invoking_call,
					message(names_sym, fn_sym, names))
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

				strs_sym <- match.call()[-1][[1]]

				assert(
					is.character(strs), invoking_call,
					message(strs_sym, strs))
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

				coll_sym <- match.call()[-1][[1]]

				assert(
					is_collection(coll), invoking_call,
					message(coll_sym, coll))
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

				coll_sym <- match.call()[-1][[1]]

				assert(
					is.recursive(coll), invoking_call,
					message(coll_sym, coll))
			}
		})

	this$must_be_longer_than <-
		local({

			message <- function (coll_sym, coll) {
				"the argument matching " %+% ddquote(coll_sym) %+%
				" must have length longer than " %+% length %+% "." %+%
				summate(coll)
			}

			function (coll, length, invoking_call) {
				# the collection must be longer than.

				coll_sym <- match.call()[-1][[1]]

				assert(
					length(coll) >= length, invoking_call,
					message(coll_sym, coll))
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

				coll_sym <- match.call()[-1][[1]]

				assert(
					length(coll) >= length, invoking_call,
					message(coll_sym, length, coll))
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

					coll1_sym <- match.call()[-1][[1]]
					coll2_sym <- match.call()[-1][[2]]

					assert(
						length(coll1) == length(coll2), invoking_call,
						message(coll1_sym, coll2_sym))
				}
			})

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

	this$must_be_invoked_with_brackets <-
		function (invoking_call) {

			message <- "comprehension objects cannot be invoked as a " %+%
				"function: theu must be invoked with square brackets ( [] )"

			assert(
				False, invoking_call, message)
		}

	this$must_be_correct_type <-
		function (coll_sym, coll, mode, invoking_call) {

			message <- "the collection " %+% ddquote(coll_sym) %+% " cannot be " %+%
			"converted to a vector of mode " %+% ddquote(mode)

			assert(
				mode %in% is(coll), invoking_call, message)
		}

	this$must_be_heterogenous <-
		local({
			# a vector must be convertable to a type.

			all_are <- list(
				integer =
					function (coll) {
						all( vapply(coll, is.integer, logical(1)) )
					},
				logical =
					function (coll) {
						all( vapply(coll, is.logical, logical(1)) )
					},
				double =
					function (coll) {
						all( vapply(coll, is.double, logical(1)) )
					},
				numeric =
					function (coll) {
						all( vapply(coll, is.numeric, logical(1)) )
					},
				character =
					function (coll) {
						all( vapply(coll, is.character, logical(1)) )
					},
				raw =
					function (coll) {
						all( vapply(coll, is.raw, logical(1)) )
					},
				complex =
					function (coll) {
						all( vapply(coll, is.complex, logical(1)) )
					}
			)

			function (coll_sym, coll, mode, invoking_call) {

				message <- "the collection " %+% ddquote(coll_sym) %+%
				" must be a collection of values of type " %+% ddquote(mode)

				assert(
					all_are[[mode]](coll), invoking_call, message)

			}
		})

	this

})


demand <- local({

	this <- Object()

	this$must_have_yield <-
		function (indices, invoking_call) {

			message <- "a collection-comprehension must not begin with a " %+%
				"variable bind expression."

			assert(
				1 %!in% indices, invoking_call, message)
		}

	this$must_be_unnamed <-
		function (exprs, invoking_call) {

			message <- "a collection-comprehension cannot have named sub-terms."

			assert(
				is.null(names(exprs)), invoking_call,
				message)
		}

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
		function (variables, invoking_call) {

			message <- "a non-empty collection-comprehension must have " %+%
			"at least one variable binding."

			assert(
				length(variables) > 0, invoking_call,
				message)
		}

	this

})
