




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





# kea_condition_message
#
# R prints the $ message field of its error objects. Kea's messages
# don't come out great by default, so some pre-processing of the message and
# call need to be done before printing.
#
# (ecall, message) -> kea_condition_message -> $ message -> stop|warning
#

kea_condition_message <- local({

	stringify_call_fn <- function (ecall) {

		calling <- ecall[[1]]

		if (is.name(calling)) {
			paste0( calling )
		} else {
			truncate(ddparse( calling ), 50)
		}

	}

	call_parts <- function (ecall) {

		list(
			fn    = stringify_call_fn(ecall),
			whole = stringify_call(ecall)
		)

	}





	function (ecall, message) {

		if (length(message) != 1) {

			stop('internal error in kea; a non-length one error message was produced.' %+%
				' Please report this at https://github.com/rgrannell1/kea/issues')

		} else {

			parts <- call_parts(ecall)

			"\n" %+% message %+%
			"\nThrown from " %+% parts $ fn %+%
			"\nIn the call " %+% parts $ whole -> unpadded_message

			gsub('\n', '\n ', unpadded_message)

		}

	}

})







# KeaCondition
#
# the constructor for kea error types.

KeaCondition <- function (...) {

	custom_classes <- c(...)

	function (ecall, message) {

		fields <- list()

		# -- the erroneous call. Represented as is. Not printed; for
		# -- programmatic use up the call stack.

		fields $ ecall    <- ecall

		# -- what the user sees. Highly processed error message.

		fields $ message <- kea_condition_message(ecall, message)

		# -- TODO more custom data to aid debugging.

		structure(class = c(custom_classes, 'kea_condition', 'condition'), fields)

	}

}





# exception
#
# the list of augmented error types used in kea. These each take a call from where the
# exception was raised, and an error message. They create an object representing the
# particular type of error.

exception <- list()

exception $ Error      <- KeaCondition('error')
exception $ Warning    <- KeaCondition('warning')

exception $ Arithmetic <- KeaCondition('error', 'Arithmetic_error')

# -- reference errors.
exception $ Lookup     <- KeaCondition('error', 'Lookup_error')
exception $ Index      <- KeaCondition('error', 'Lookup_error', 'Index_error')
exception $ Key        <- KeaCondition('error', 'Lookup_error', 'key_error')

# -- read / write errors.
exception $ Io         <- KeaCondition('error', 'Lookup_error', 'io_error')

# -- variable lookup fails.
exception $ Name       <- KeaCondition('error', 'Lookup_error', 'name_error')

# -- throw an error for custom syntax.
exception $ Syntax     <- KeaCondition('error', 'syntax_error')

# -- none specific error types.
exception $ Type       <- KeaCondition('error', 'type_error')
exception $ Value      <- KeaCondition('error', 'value_error')





# keaThrower
#
# create equivalents of stop for each kea exception type.
# the result takes a call from which the error was thrown,
# an error message, creates a lovely kea error, and throws it.
#
# the constructor for kea errors nicely formats the error message
# to be thrown and includes additional data for debuggin.

keaThrower <- function (constructor) {
	function (invoking_call, message) {

		# -- the kea warning or error to throw.
		cond <- constructor(invoking_call, message)

		base_thrower <- if (inherits(cond, 'warning')) {

			cond $ message <- colourise $ yellow(cond $ message)
			warning

		} else if (inherits(cond, 'error')) {

			cond $ message <- colourise $ red(cond $ message)
			stop

		}

		base_thrower(cond)

	}
}






# throw_exception
#
# the things actually called by kea code; the public portion of this file.
# Each field contains a function that takes a call and a message, and when
# called throws a lovely custom error.

throw_exception <- list(
	warning          = keaThrower(exception $ Warning),
	error            = keaThrower(exception $ Error),

	arithmetic_error = keaThrower(exception $ Arithmetic_error),

	lookup_error     = keaThrower(exception $ Lookup),
	index_error      = keaThrower(exception $ Index),
	key_error        = keaThrower(exception $ Key),
	io_error         = keaThrower(exception $ Io),
	name_error       = keaThrower(exception $ Name),
	syntax_error     = keaThrower(exception $ Syntax),
	type_error       = keaThrower(exception $ Type),
	value_error      = keaThrower(exception $ Value)
)
