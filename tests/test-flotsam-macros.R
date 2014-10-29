
require(kea)

'%+%' <- kea ::: '%+%'
throw_exception <- kea ::: throw_exception
is_na <- kea:::is_na

# -- This series of unit tests checks that files that
# -- should exist, do exist.

kea_function_regexp <- 'x[A-Z].+[a-z]$'
kea_env <- as.environment('package:kea')

fn_calls <- body := {
	# -- get every function call in the body.

	all_vars <- all.names(body)
	not_fns  <- all.vars(body, unique = False)

	all_vars[!(all_vars %in% not_fns)]
}



# -- TODO ADD TEST To make sure every Return function and variadic function has
# -- a matching roxygen tag.


# -- select the name and every function in kea.

kea_fns <-
	x_(ls('package:kea')) $
	xSelect(xIsMatch(kea_function_regexp)) $
	xMap(fn_name := {
		list( fn_name, kea_env[[fn_name]] )
	})

inner_calls <- kea_fns $ xMap( xUnspread((fn_name : fn) := {
	list( fn_name, fn, fn_calls(body(fn)) )
}) )

message(
	'test that no forbidden functions are ever called')

	forbidden_calls <- calls := {
		x_(calls) $ xIntersect_(c('sample', 'is.na', 'is.nan', 'is.atomic', 'is.list', 'match.fn', 'unlist'))
	}

	# -- select the functions that make dangerous calls.

	error_message <-
		inner_calls $
		xSelect( xUnspread((fn_name : fn : calls) := {
			forbidden_calls(calls) $ x_NotEmpty()
		}) ) $
		# -- create the error message for the functions with dangerous calls.
		xMap( xUnspread((fn_name : fn : calls) := {

			xFromWords_(
				"the function", fn_name,
				"makes calls to the following bad functions:",
				forbidden_calls(calls) $ x_Implode(', ')
			)

		}) ) $
		# -- collapse into one message.
		x_FromLines()

	if (isTRUE(nchar(error_message) > 0)) {
		warning(message = error_message)
	}


