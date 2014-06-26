
#
# this could do with a dose of code review and commenting; it is core code.

# ----------------------------------------------------------------------------
# Accessory Functions
#
# These functions reformat the call data passed to a string.

stringify_call <- function (call) {
	# call -> string
	# format the call nicely for printing, fixing the representation of ':='.

	if (length(call) == 0) {
		"erroneous call not included"
	} else {

		call <- as.call(lapply(call, function (term) {

			# -- refactor `:=`(a, {a + a}) to function (a) {a + a}
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

	if (length(invoking_call) == 1 && any(class(invoking_call) == "call")) {
		# -- is it a nullary call ( foo() )

		fn <- invoking_call[[1]]

		fn_text <- if (is.name(fn)) {
			paste(fn)
		} else {
			# -- calling position may be a function literal occasionally.
			# -- can be quite large, so truncate

			tmp <- ddparse(fn)

			if (nchar(tmp) > 50) {
				paste0(substring(tmp, 1, 50), ' [truncated]', collapse = '')
			} else {
				tmp
			}
		}

		list(
			invoking_call =
				fn_text,
			calltext =
				paste0(invoking_call, "()", collapse = ''))

	} else if (any(class(invoking_call) == "call")) {
		# -- the general case

		fn <- invoking_call[[1]]

		fn_text <- if (is.name(fn)) {
			paste(fn)
		} else {
			# -- calling position may be a function literal occasionally.
			# -- can be quite large, so truncate

			tmp <- ddparse(fn)

			if (nchar(tmp) > 50) {
				paste0(substring(tmp, 1, 50), ' [truncated]', collapse = '')
			} else {
				tmp
			}
		}

		list(
			invoking =
				fn_text,
			calltext =
				stringify_call(invoking_call))

	} else {
		# -- internal error; something bad was passed in.

		list(
			invoking =
				"",
			calltext =
				"")

	}
}

# -------------------------------------------------------------------------
#
# DEPRECATE ME
#
# Error colourisers
#
# This takes your message, and throws either an error or warning.

write_error <- function (..., call. = True) {
	# -- take a string, colourise it and throw an error
	message <- c(...)

	stop(colourise$red(message), call. = call.)
}

# -------------------------------------------------------------------------
#
# Top Level Interface
#
# This takes your message, and throws either an error or warning.

throw_kiwi_warning <- function (invoking_call, message) {
	# the top level interface to throwing an kiwi error.

	# -- stringify the call, get the function name.
	if (!missing(invoking_call)) {
		components <- get_call_components(invoking_call)

		# -- the function foo, and the stringified call foo(baz, bar, ...)
		callname <- components$invoking
		calltext <- components$calltext

		# -- just in case
		callname <- paste0(callname, collapse = '')
		calltext <- wrap(calltext, indent = 4)

		# -- these few lines dicate how an kiwi error message will be formatted

		final_message <-
		"\n" %+% message %+%
		"\nThrown from " %+% callname %+% "\n" %+%
		"In the call " %+% calltext
	} else {
		final_message <- "\n" %+% message
	}

	# -- tput as red (if possible) and report the error.

	warning(colourise$yellow(final_message), call. = False)

}

throw_kiwi_error <- function (invoking_call, message) {
	# the top level interface to throwing an kiwi error.

	# -- stringify the call, get the function name.
	# -- get the function foo, and the stringified call foo(baz, bar, ...)

	if (!missing(invoking_call)) {
		components <- get_call_components(invoking_call)

		callname <- components$invoking
		calltext <- components$calltext

		# -- just in case
		callname <- paste0(callname, collapse = '')
		calltext <- wrap(calltext, indent = 0)

		# -- these few lines dicate how an kiwi error message will be formatted

		final_message <-
		"\n" %+% message %+%
		"\nThrown from " %+% callname %+% "\n" %+%
		"In the call " %+% calltext
	} else {
		final_message <- "\n" %+% message
	}

	# -- tput as red (if possible) and report the error.

	stop(colourise$red(final_message), call. = False)

}

# -------------------------------------------------------------------------
#
# Try Functions
#
# There wrap dangerous tasks, like reading and writing.
#

try_read <- local({
	function (expr, path, invoking_call) {

		tryCatch(
			expr,
			warning = function (warn) {

				warnmessage <-
					paste0(warn$message, collapse = '')

				overview <-
				"A warning occurred while reading from the path " %+% dQuote(path) %+% ":\n\n"

				inner_call <- stringify_call(warn$call) %+% ':\n\n'


				# -- add padding to the front of the message.
				inner_call <- paste0('    ', inner_call)

				warnmessage <- strsplit(warnmessage, '\n')[[1]]
				warnmessage <- paste0('    ', warnmessage, collapse = '\n')

				throw_kiwi_warning(
					overview %+% inner_call %+% warnmessage, invoking_call)

			},
			error = function (err) {
				# -- path is scoped lexically.

				errmessage <-
					paste0(err$message, collapse = '')

				overview <-
				"An error occurred while reading from the path " %+% dQuote(path) %+% "\n\n"

				inner_call <- stringify_call(err$call) %+% ':\n\n'

				# -- add padding to the front of the message.
				inner_call <- paste0('    ', inner_call)

				errmessage <- strsplit(errmessage, '\n')[[1]]
				errmessage <- paste0('    ', errmessage, collapse = '\n')

				throw_kiwi_error(
					overview %+% inner_call %+% errmessage, invoking_call)
			}
		)
	}

})

try_write <- local({
	function (expr, path, invoking_call) {

		tryCatch(
			expr,
			warning = function (warn) {
				# -- path is lexically scoped to here


				errmessage <-
					paste0(warn$message, collapse = '')

				# -- this must be changed when the error style is changed.
				overview <-
				"A warning occurred while writing to the path " %+% dQuote(path) %+% "\n\n"

				inner_call <- stringify_call(warn$call) %+% ':\n\n'

				# -- add padding to the front of the message.
				inner_call <- paste0('    ', inner_call)

				warnmessage <- strsplit(warnmessage, '\n')[[1]]
				warnmessage <- paste0('    ', warnmessage, collapse = '\n')

				warning(
					colourise$yellow(overview %+% inner_call %+% warnmessage),
					call. = False)

			},
			error = function (err) {
				# -- path is lexically scoped to here

				errmessage <-
					paste0(err$message, collapse = '')

				# -- this must be changed when the error style is changed.
				overview <-
				"An error occurred while writing to the path " %+% dQuote(path) %+% "\n\n"

				inner_call <- stringify_call(err$call) %+% ':\n\n'

				# -- add padding to the front of the message.
				inner_call <- paste0('    ', inner_call)

				errmessage <- strsplit(errmessage, '\n')[[1]]
				errmessage <- paste0('    ', errmessage, collapse = '\n')

				warning(
					colourise$red(overview %+% inner_call %+% errmessage),
					call. = False)

			}
		)
	}

})

# Ensure that a regular expression doesn't fail.
#
# Intercept a message from R's internal regexp validation,
# relabel as comming from top-level kiwi function.
#
#

check_regexp <- function (rexp, invoking_call) {

	tryCatch(
		regexpr(rexp, text = ''),
		warning = function (warn) {
			message <- err $ message %+%
			'\n'

			throw_kiwi_warning(invoking_call, message)
		},
		error = function (err) {
			message <- err $ message %+%
			'\n'

			throw_kiwi_error(invoking_call, message)
		}
	)
}
