
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

	if (length(invoking_call) == 1 && any(class(invoking_call) == "call")) {
		# -- is it a nullary call ( foo() )

		list(
			invoking_call =
				paste0(invoking_call, collapse = ''),
			calltext =
				paste0(invoking_call, "()", collapse = ''))

	} else if (any(class(invoking_call) == "call")) {
		# -- the general case

		list(
			invoking =
				paste0(invoking_call[[1]], collapse = ''),
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

write_error <- function (..., call. = True) {

	message <- c(...)

	stop(colourise$red(message), call. = call.)
}

assert <- function (expr, invoking_call, message) {

	if (!is.logical(expr)) {
		# -- the assertion was broken.

		message <-
			"internal error: the assertion " %+% ddparse(expr) %+%
			" produced a non-logical value."

		write_error(message, call. = False)

	} else if (!isTRUE(expr)) {
		# -- everything went wrong, throw an error.

		components <- get_call_components(invoking_call)

		write_error(
			exclaim$arrow_function_failed(
				components$invoking, components$calltext, message),
			call. = False)

	}
	True
}




throw_arrow_error <- function (invoking_call, message) {
	# everythings went wrong, throw an error.

	components <- get_call_components(invoking_call)

	write_error(
		exclaim$arrow_function_failed(
			components$invoking, components$calltext, message),
			call. = False)
}


try_write <- local({
	function (expr, path, invoking_call) {

		tryCatch(
			expr,
			warning = function (warn) {
				apically_calling_fn <- invoking_call[[1]]

				write_warning(
					exclaim$arrow_function_failed(
						components$invoking, components$calltext, message),
					call. = False)
			},
			error = function (err) {
				apically_calling_fn <- invoking_call[[1]]

				write_error(
					exclaim$arrow_function_failed(
						components$invoking, components$calltext, message),
					call. = False)
			}
		)
	}

})

try_read <- local({
	function (expr, path, invoking_call) {

		tryCatch(
			expr,
			warning = function (warn) {
				apically_calling_fn <- invoking_call[[1]]

				assert(
					False, invoking_call,
					exclaim$warning_read(path, warn)
				)
			},
			error = function (err) {
				apically_calling_fn <- invoking_call[[1]]

				assert(
					False, invoking_call,
					exclaim$error_read(path, err)
				)
			}
		)
	}

})

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
