
# -- This suite of unit tests checks that
# -- functions make calls to essential functions like missing,
# -- and the macro functions.

# -- select the name and every function in kiwi.

require(kiwi)

kiwi_function_regexp <- 'x[A-Z].+[a-z]$'
kiwi_env <- as.environment('package:kiwi')

kiwi_fns <-
	x_(ls('package:kiwi')) $
	xSelect(
		xFix(xIsMatch, kiwi_function_regexp)) $
	xMap(fn_name := {
		list( fn_name, kiwi_env[[fn_name]] )
	})

inner_vars <- kiwi_fns $ xMapply((fn_name : fn) := {
	list( fn_name, fn, all.names(body(fn)) )
})

message("test that the is_fn_matchable macro is used for all functions")

message("test that the is_collection macro is used for all collections")

