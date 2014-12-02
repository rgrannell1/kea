
# kea_condition_message
#
# R prints the $ message field of its error objects. Kea's messages
# don't come out great by default, so some pre-processing of the message and
# call need to be done before printing.
#
# (ecall, message) -> kea_condition_message -> $ message -> stop|warning
#

kea_condition_message <- function (ecall, message) {

}






# KeaCondition
#
# the constructor for kea error types.

KeaCondition <- function (...) {

	custom_classes <- c(...)

	function (ecall, message) {

		fields <- list()

		# -- the erroneous call. Represented as is. Not printed; for
		# -- programmatic use up the call stack.

		fields $ call    <- ecall

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
			warning
		} else if (inherits(cond, 'error')) {
			stop
		}

		base_thrower(cond, call. = False)

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
