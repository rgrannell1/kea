
#
# this could do with a dose of code review and commenting; it is core code.

# ----------------------------------------------------------------------------
# Accessory Functions
#
# These functions reformat the call data passed to a string.



expand_call <- function (call) {
	# -- expand calls to := to their evaluated functions.
	# -- refactor `:=`(a, {a + a}) to function (a) {a + a}

	is_lambda <- function (term) {
		is.call(term) && term[[1]] == as.symbol(':=')
	}

	as.call( lapply(call, function (term) {
		if (is_lambda(term)) eval(term) else term
	}) )

}

stringify_call <- function (call) {
	# call -> string
	# format the call nicely for printing, fixing the representation of ':='.

	if (length(call) == 0) {
		"[erroneous call not included]"
	} else {

		calltext <- ddparse(expand_call(call))

		if (nchar(calltext) <= 50) {
			calltext
		} else {
			paste0(substring(calltext, 1, 50), ' [truncated]', collapse = '')
		}

	}
}

get_call_components <- function (invoking_call) {
	# get the calling function and call text from a call.

	if (length(invoking_call) == 1 && any(class(invoking_call) == "call")) {
		# -- is it a nullary call ( foo() )

		fn <- invoking_call[[1]]

		calltext <- paste0(invoking_call, "()", collapse = '')
		fntext   <- if (is.name(fn)) {
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

		list(invoking_call = fntext, calltext = calltext)

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
			invoking = fn_text,
			calltext = stringify_call(invoking_call))

	} else {
		# -- internal error; something bad was passed in.

		list(invoking = "", calltext = "")

	}
}

# -------------------------------------------------------------------------
#
# Top Level Interface
#
# This takes your message, and throws either an error or warning.

throw_kea_condition <- function (exception) {

	function (invoking_call, message) {

		if (length(message) != 1) {
			stop('internal error in kea; a non-length one error message was produced.' %+%
				' Please report this at https://github.com/rgrannell1/kea/issues')
		}

		# -- stringify the call, get the function name.
		final_message <- if (!missing(invoking_call)) {

			call_components <- get_call_components(invoking_call)

			"\n" %+% message %+%
			"\nThrown from " %+% call_components $ invoking %+% "\n" %+%
			"In the call "   %+% call_components $ calltext

		} else {
			"\n" %+% message
		}

		exception(gsub('\n', '\n ', final_message))
	}
}





# -- Create a new subclass of error, and a matching function
# -- to raise that exception.

new_error_type <- function (...) {

	classes <- c(...)

	function (message) {
		structure(
			class = c(classes, 'condition', 'error'),
			list(message = paste0(classes[length(classes)], ': ', message))
		)
	}

}




# -- Python-Style error types. Should allow easier distinguishing between
# -- types of error.

arithmetic_error <- new_error_type('arithmetic_error')

# -- reference errors.
lookup_error     <- new_error_type('lookup_error')
index_error      <- new_error_type('lookup_error', 'index_error')
key_error        <- new_error_type('lookup_error', 'key_error')

# -- read / write errors.
io_error         <- new_error_type('lookup_error', 'io_error')

# -- variable lookup fails.
name_error       <- new_error_type('lookup_error', 'name_error')

# -- throw an error for custom syntax.
syntax_error     <- new_error_type('syntax_error')

# -- none specific error types.
type_error       <- new_error_type('type_error')
value_error      <- new_error_type('value_error')



raise_error   <- function (condition, colour) {
	function (message) {
		stop( condition(colourise [[colour]](message) ))
	}
}

raise_warning <- function (condition, colour) {
	throw_kea_condition(function (message) {
		warning( condition(colourise [[colour]](message) ))
	})
}





throw_exception <- list(
	warning          = raise_warning(identity,       'yellow'),
	error            = raise_error(identity,         'red'),

	arithmetic_error = raise_error(arithmetic_error, 'red'),

	lookup_error     = raise_error(lookup_error,     'red'),
	index_error      = raise_error(index_error,      'red'),
	key_error        = raise_error(key_error,        'red'),
	io_error         = raise_error(io_error,         'red'),
	name_error       = raise_error(name_error,       'red'),
	syntax_error     = raise_error(syntax_error,     'red'),
	type_error       = raise_error(type_error,       'red'),
	value_error      = raise_error(value_error,      'red')
)

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
					paste0(warn $ message, collapse = '')

				overview <-
				"A warning occurred while reading from the path " %+% dQuote(path) %+% ":\n\n"

				inner_call <- stringify_call(warn$call) %+% ':\n\n'

				# -- add padding to the front of the message.
				inner_call <- paste0('    ', inner_call)

				warnmessage <- strsplit(warnmessage, '\n')[[1]]
				warnmessage <- paste0('    ', warnmessage, collapse = '\n')

				throw_exception $ warning(
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

				throw_exception $ error(
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


				warnmessage <-
					paste0(warn $ message, collapse = '')

				# -- this must be changed when the error style is changed.
				overview <-
					"A warning occurred while writing to the path " %+% dQuote(path) %+% "\n\n"

				inner_call <- stringify_call(warn $ call) %+% ':\n\n'

				# -- add padding to the front of the message.
				inner_call <- paste0('    ', inner_call)

				warnmessage <- strsplit(warnmessage, '\n')[[1]]
				warnmessage <- paste0('    ', warnmessage, collapse = '\n')

				warning(
					colourise $ yellow(overview %+% inner_call %+% warnmessage),
					call. = False)

			},
			error = function (err) {
				# -- path is lexically scoped to here

				errmessage <-
					paste0(err $ message, collapse = '')

				# -- this must be changed when the error style is changed.
				overview <-
					"An error occurred while writing to the path " %+% dQuote(path) %+% "\n\n"

				inner_call <- stringify_call(err $ call) %+% ':\n\n'

				# -- add padding to the front of the message.
				inner_call <- paste0('    ', inner_call)

				errmessage <- strsplit(errmessage, '\n')[[1]]
				errmessage <- paste0('    ', errmessage, collapse = '\n')

				warning(
					colourise $ red(overview %+% inner_call %+% errmessage),
					call. = False)

			}
		)
	}

})












# Ensure that a regular expression doesn't fail.
#
# Intercept a message from R's internal regexp validation,
# relabel as comming from top-level kea function.
#
#

check_regexp <- function (rexp, invoking_call) {

	tryCatch(
		if (length(rexp) > 0) {
			regexpr(rexp, text = '')
		},
		warning = function (war) {
			throw_exception $ warning(invoking_call, war $ message %+% '\n')
		},
		error = function (err) {
			throw_exception $ error(invoking_call,   err $ message %+% '\n')
		}
	)

}
