
require(kiwi)

'%+%' <- kiwi ::: '%+%'
throw_kiwi_error <- kiwi ::: throw_kiwi_error
is_na <- kiwi:::is_na

# -- This series of unit tests checks that files that
# -- should exist, do exist.

kiwi_function_regexp <- 'x[A-Z].+[a-z]$'
kiwi_env <- as.environment('package:kiwi')

fn_calls <- body := {
	# -- get every function call in the body.

	all_vars <- all.names(body)
	not_fns  <- all.vars(body, unique = False)

	all_vars[!(all_vars %in% not_fns)]
}



# -- TODO ADD TEST To make sure every Return function and variadic function has
# -- a matching roxygen tag.


# -- select the name and every function in kiwi.

kiwi_fns <-
	x_(ls('package:kiwi')) $
	xSelect(xIsMatch(kiwi_function_regexp)) $
	xMap(fn_name := {
		list( fn_name, kiwi_env[[fn_name]] )
	})

inner_calls <- kiwi_fns $ xMapply((fn_name : fn) := {
	list( fn_name, fn, fn_calls(body(fn)) )
})

message(
	'test that no forbidden functions are ever called')

	forbidden_calls <- calls := {
		x_(calls) $ xInterOf_(c('sample', 'is.na', 'is.nan', 'is.atomic', 'is.list', 'match.fn'))
	}

	# -- select the functions that make dangerous calls.

	error_message <-
		inner_calls $
		xSelect( xUnspread((fn_name : fn : calls) := {
			forbidden_calls(calls) $ x_NotEmpty()
		}) ) $
		# -- create the error message for the functions with dangerous calls.
		xMapply((fn_name : fn : calls) := {

			xFromWords_(
				"the function", fn_name,
				"makes calls to the following bad functions:",
				forbidden_calls(calls) $ x_Implode(', ')
			)

		}) $
		# -- collapse into one message.
		x_FromLines()

	if (isTRUE(nchar(error_message) > 0)) {
		throw_kiwi_error(message = error_message)
	}


