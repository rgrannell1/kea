
#
# Try Functions
#
# There wrap dangerous tasks, like reading and writing.
#
# TODO FIX UP THIS REALLY OLD, CRAPPY CODE
# TODO FIX UP THIS REALLY OLD, CRAPPY CODE
# TODO FIX UP THIS REALLY OLD, CRAPPY CODE
# TODO FIX UP THIS REALLY OLD, CRAPPY CODE
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
