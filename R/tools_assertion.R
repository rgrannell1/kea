
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

		if (nchar(calltext) > 50) {
			paste0(substring(calltext, 1, 50), ' [truncated]', collapse = '')
		} else {
			calltext
		}
	}
}

get_call_components <- function (invoking_call) {
	# get the calling function and call text from a call.

	if (length(invoking_call) == 1) {
		# this may be incorrect - I'm assuming this is a
		# length-one call (xMap()) , not a symbol.

		list(
			invoking_call =
				paste0(invoking_call, collapse = ''),
			calltext =
				paste0(invoking_call, "()", collapse = ''))
	} else {
		list(
			invoking =
				paste0(invoking_call[[1]], collapse = ''),
			calltext =
				stringify_call(invoking_call))
	}
}

write_error <- function (..., call. = True) {
	# to fix wrong terminal type
	# sudo nano ~/.bashrc
	# export TERM=term-color
	# . ~/.bashenv

	message <- c(...)

	stop(colourise$red(message), call. = call.)

}

assert <- local({

	function (expr, invoking_call, message) {
		# does an expression evaluate to true?
		# if not, throw a lovely error.

		if (!is.logical(expr)) {
			# the assertion was broken.

			message <-
				"internal error: the assertion " %+% ddparse(expr) %+%
				" produced a non-logical value."

			write_error(message, call. = False)

		} else if (!isTRUE(expr)) {
			# everythings went wrong, throw an error.

			components <- get_call_components(invoking_call)

			write_error(
				exclaim$arrow_function_failed(
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
		exclaim$arrow_function_failed(
			components$invoking, components$calltext, message),
			call. = False)
}





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
						exclaim$arrow_function_failed(
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
						exclaim$arrow_function_failed(
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




























































