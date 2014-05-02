

# -------------------------------- exclaim -------------------------------- #
#
# To Developers,
#
# exclaim stores the error messages specific to assert itself.

exclaim <- list(
	arrow_function_failed =
		function (callname, callinfo, message) {

			stopifnot(is.character(message))

			callname <- paste0(callname, collapse = '')
			callinfo <- wrap(callinfo, indent = 4)

			overview <-
			'\n[ error thrown from ' %+% callname %+% ' ]:\n\n'

			overview %+%
			callinfo %+% '\n\n' %+%
			'[ details ]:\n\n' %+%
			message

		},

	warning_higher_order =
		function (fn, warn, profile = '') {

			warnmessage <-
				paste0(warn$message, collapse = '')

			inner_call <- stringify_call(warn$call) %+% ":\n\n"

			overview <-
			"[ warning occurred while executing a function passed to " %+% fn %+% " ]\n\n"

			inner_call <- paste0('    ', inner_call)

			warnmessage <- strsplit(warnmessage, '\n')[[1]]
			warnmessage <- paste0('    ', warnmessage, collapse = '\n')

			overview %+% inner_call %+% warnmessage
		},

	error_higher_order =
		function (fn, err, profile = '') {

			errmessage <-
				paste0(err$message, collapse = '')

			inner_call <- stringify_call(err$call) %+% ':\n\n'

			overview <-
			"[ an error occurred while executing a function passed to " %+% fn %+% " ]:\n\n"

			inner_call <- paste0('    ', inner_call)

			errmessage <- strsplit(errmessage, '\n')[[1]]
			errmessage <- paste0('    ', errmessage, collapse = '\n')

			overview %+% inner_call %+% errmessage
		},

	warning_write =
		function (path, err, profile = '') {

			errmessage <-
				paste0(err$message, collapse = '')

			overview <-
			"[ a warning occurred while writing to the path " %+% dQuote(path) %+% " ]:\n\n"

			inner_call <- stringify_call(err$call) %+% ':\n\n'

			inner_call <- paste0('    ', inner_call)

			errmessage <- strsplit(errmessage, '\n')[[1]]
			errmessage <- paste0('    ', errmessage, collapse = '\n')

			overview %+% inner_call %+% errmessage
		},
	error_write =
		function (path, err, profile = '') {

			errmessage <-
				paste0(err$message, collapse = '')

			overview <-
			"[ an error occurred while writing to the path " %+% dQuote(path) %+% " ]:\n\n"

			inner_call <- stringify_call(err$call) %+% ':\n\n'

			inner_call <- paste0('    ', inner_call)

			errmessage <- strsplit(errmessage, '\n')[[1]]
			errmessage <- paste0('    ', errmessage, collapse = '\n')

			overview %+% inner_call %+% errmessage
		},

	warning_read =
		function (path, warn, profile = '') {

			warnmessage <-
				paste0(warn$message, collapse = '')

			overview <-
			"[ a warning occurred while reading from the path " %+% dQuote(path) %+% " ]:\n\n"

			inner_call <- stringify_call(warn$call) %+% ':\n\n'

			inner_call <- paste0('    ', inner_call)

			warnmessage <- strsplit(warnmessage, '\n')[[1]]
			warnmessage <- paste0('1    ', warnmessage, collapse = '\n')

			overview %+% inner_call %+% warnmessage
		},
	error_read =
		function (path, err, profile = '') {

			errmessage <-
				paste0(err$message, collapse = '')

			overview <-
			"[ an error occurred while reading from the path " %+% dQuote(path) %+% " ]:\n\n"

			inner_call <- stringify_call(err$call) %+% ':\n\n'

			inner_call <- paste0('    ', inner_call)

			errmessage <- strsplit(errmessage, '\n')[[1]]
			errmessage <- paste0('    ', errmessage, collapse = '\n')

			overview %+% inner_call %+% errmessage
		}
)
