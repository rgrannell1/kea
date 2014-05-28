
# -- This suite of unit tests checks that
# -- functions make calls to essential functions like missing,
# -- and the macro functions.

# -- select the name and every function in arrow.

arrow_function_regexp <- 'x[A-Z].+[a-z]$'
arrow_env <- as.environment('package:arrow')

arrow_fns <-
	x_(ls('package:arrow')) $
	xSelect(
		xFix(xIsMatch, arrow_function_regexp)) $
	xMap(fn_name := {
		list( fn_name, arrow_env[[fn_name]] )
	})

inner_vars <- arrow_fns $ xMapply((fn_name : fn) := {
	list( fn_name, fn, all.names(body(fn)) )
})

message('test that every function checks if its parametres are missing')

	inner_vars $
	xMapply((fn_name : fn : vars) := {

		# -- generate a sliding window of var name pairs
		# -- (var names are ordered).

		pairs <- x_(1:length(vars)) $ xMap(ith := {
			xTake(2, xFirstOf( xCycle_(ith, vars)) )
		})

		# -- for each parametre of a functions,
		# -- reject those that have a missing
		# -- function followed by the parametre name

		param_missing_check <-
			x_(xParamsOf(fn)) $ xReject(x. == '...') $ x_Reject(param := {
				pairs $ x_AnyOf(pair := {
					xFirstOf(pair) == 'missing' && xSecondOf(pair) == param
				})
			})

		# -- report functions with missing parametres.

		if (xNotEmpty(param_missing_check)) {
			list( fn_name, xFromWords_(fn_name, '(', toString(param_missing_check), ')') )
		}

	}) $
	xSelect(xNotEmpty) $ xReject(pair := {
		xIsMember_(xFirstOf(pair), 'xFromChars', 'xFromWords', 'xFromLines', 'xLambda')
	}) $
	# -- select the messages
	xAtCol(2) $ xFromLines() $ xDo(miss := {
		stop(xFromLines_('the following functions are missing missing parametre checks.', miss))
	})
