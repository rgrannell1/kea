
	require(arrow)

	'%+%' <- arrow ::: '%+%'
	throw_arrow_error <- arrow ::: throw_arrow_error

	# -- This series of unit tests checks that files that
	# -- should exist, do exist.

	arrow_function_regexp <- 'x[A-Z].+[a-z]$'
	arrow_env <- as.environment('package:arrow')

	fn_calls <- body := {
		# -- get every function call in the body.

		all_vars <- all.names(body)
		not_fns  <- all.vars(body, unique = False)

		all_vars[!(all_vars %in% not_fns)]
	}





	# -- select the name and every function in arrow.

	arrow_fns <-
		x_(ls('package:arrow')) $
		xSelect(
			xFix(xIsMatch, arrow_function_regexp)) $
		xMap(fn_name := {
			list( fn_name, arrow_env[[fn_name]] )
		})

	inner_calls <- arrow_fns $ xMapply((fn_name : fn) := {
		list( fn_name, fn, fn_calls(body(fn)) )
	})

	message(
		'test that no forbidden functions are ever called')

		forbidden_calls <- calls := {
			x_(calls) $ xInter_(c('sample'))
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

		throw_arrow_error(message = error_message)

	message(
		'test that every function with ',
		'parametres calls the "missing" macro')

		# -- get the function calls


