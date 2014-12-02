
#
# this could do with a dose of code review and commenting; it is core code.

# ----------------------------------------------------------------------------
# Accessory Functions
#
# These functions reformat the call data passed to a string.





# truncate
#
# string -> number -> string
#
# cut a string to a fixed length, and indicate that the string was truncated.

truncate <- function (str, num) {

	if (nchar(str) > num) {
		paste0(substring(str, 1, num), '[truncated]', collapse = '')
	} else {
		str
	}

}





# stringify_call
#
# call -> string
#
# get the string representation of a call object.

stringify_call <- local({

	# expand_call
	#
	# call -> call
	#
	# expand calls to := to their evaluated functions.
	# refactor `:=`(a, {a + a}) to function (a) {a + a}

	expand_call <- function (call) {

		is_lambda <- function (term) {
			is.call(term) && term[[1]] == as.symbol(':=')
		}

		as.call( lapply(call, function (term) {
			if (is_lambda(term)) eval(term) else term
		}) )

	}





	function (call) {

		if (length(call) == 0) {
			"[erroneous call not included]"
		} else {
			truncate( ddparse(expand_call(call)) , 50)
		}

	}

})











throw_kea_condition <- local({

	stringify_calling_function <- function (invoking_call) {

		if (is.name( invoking_call[[1]] )) {
			paste0( invoking_call[[1]] )
		} else {
			truncate(ddparse( invoking_call[[1]] ), 50)
		}

	}

	get_call_components <- function (invoking_call) {

		list(
			call_text  = stringify_call(invoking_call),
			calling_fn = stringify_calling_function(invoking_call))

	}





	function (throw_exception) {

		function (invoking_call, message) {

			if (length(message) != 1) {
				stop('internal error in kea; a non-length one error message was produced.' %+%
					' Please report this at https://github.com/rgrannell1/kea/issues')
			}

			# -- stringify the call, get the function name.
			final_message <- if (!missing(invoking_call)) {

				call_components <- get_call_components(invoking_call)

				"\n" %+% message %+%
				"\nThrown from " %+% call_components $ calling_fn %+% "\n" %+%
				"In the call "   %+% call_components $ call_text

			} else {
				"\n" %+% message
			}

			throw_exception(invoking_call, gsub('\n', '\n ', final_message))

		}
	}

})






# -------------------------------------------------------------------------
#
# -- Create a new subclass of error, and a matching function
# -- to raise that exception.
#
#

Error <- function (call, message) {

	structure(
		class = c('condition', 'error'),
		list(
			message = message,
			rcall    = call
		)
	)

}

Warning <- function (call, message) {

	structure(
		class = c('condition', 'warning'),
		list(
			message = message,
			rcall   = call
		)
	)

}


new_error_type <- function (...) {

	classes <- c(...)

	function (call, message) {

		structure(
			class = c(classes, 'condition', 'error'),
			list(
				message  = paste0(classes[length(classes)], ': ', message),

				rmessage = message,
				rcall    = call
			)
		)

	}

}





exception <- list()

exception $ Error      <- Error
exception $ Warning    <- Warning

# -- new subclasses of error.

exception $ Arithmetic <- new_error_type('Arithmetic_error')

# -- reference errors.
exception $ Lookup     <- new_error_type('Lookup_error')
exception $ Index      <- new_error_type('Lookup_error', 'Index_error')
exception $ Key        <- new_error_type('Lookup_error', 'key_error')

# -- read / write errors.
exception $ Io         <- new_error_type('Lookup_error', 'io_error')

# -- variable lookup fails.
exception $ Name       <- new_error_type('Lookup_error', 'name_error')

# -- throw an error for custom syntax.
exception $ Syntax     <- new_error_type('syntax_error')

# -- none specific error types.
exception $ Type       <- new_error_type('type_error')
exception $ Value      <- new_error_type('value_error')





# #

raise_custom_error <- function (constructor) {

	throw_kea_condition(function (call, message) {
		stop( constructor(call, colourise [['red']](message) ))
	})
}

#
#

raise_custom_warning <- function (constructor) {

	throw_kea_condition(function (call, message) {
		warning( constructor(call, colourise [['yellow']](message) ))
	})
}





throw_exception <- list(
	warning          = raise_custom_warning(exception $ Warning),
	error            = raise_custom_error(  exception $ Error),

	arithmetic_error = raise_custom_error(  exception $ Arithmetic_error),

	lookup_error     = raise_custom_error(  exception $ Lookup),
	index_error      = raise_custom_error(  exception $ Index),
	key_error        = raise_custom_error(  exception $ Key),
	io_error         = raise_custom_error(  exception $ Io),
	name_error       = raise_custom_error(  exception $ Name),
	syntax_error     = raise_custom_error(  exception $ Syntax),
	type_error       = raise_custom_error(  exception $ Type),
	value_error      = raise_custom_error(  exception $ Value)
)










# -------------------------------------------------------------------------
#
# Try Functions
#
# There wrap dangerous tasks, like reading and writing.
#
# TODO FIX UP THIS REALLY OLD, CRAPPY CODE

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
