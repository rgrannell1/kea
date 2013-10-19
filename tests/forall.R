
forall <- function (cases, expect, given, max_time = 0.1) {

	pcall <- sys.call()

	assert(
		all( sapply(cases, is.function) ), pcall,
		lament$non_function_cases()
	)

	# ----- capture the expect and given expressions as functions

	expect_expr <- match.call()$expect
	given_expr <- 
		if (missing(given)) {
			True 
		} else {
			match.call()$given
		}

	expect <- given <- 
		function () {}

	formals(expect) <-
		as_parametres(names(cases))

	formals(given) <-
		as_parametres(names(cases))

	body(expect) <- expect_expr
	body(given) <- given_expr
	
	# ----- 

	state <- list(
		tests_run = 0,
		failed_after = Inf,
		time_left = xTimer(max_time),
		failed = list())

	while (state$time_left()) {

		case <- lapply(cases, function (fn) fn())

		if (do.call(given, case)) {
			
			state$tests_run <- state$tests_run + 1
			result <- do.call(expect, case)

			assert(
				result %in% c(True, False), pcall,
				lament$non_boolean_expectation(case))

			if (!result) {
				state$failed_after <- 
					min(state$failed_after, state$tests_run)

				state$failed <- 
					c(state$failed, list(case))
			}
		}
	}

	assert(
		length(state$failed) == 0,
		pcall,
		lament$failed_cases(
			state$failed_after, 
			state$failed))
}



forall(
	list(xs = function () 1, ys = function () 2),
	True
)


















