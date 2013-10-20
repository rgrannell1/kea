
G <- local({

	gcombine <- function (...) {

		fns <- list(...)

		function () {
			sample(fns, size = 1)[[1]]()
		}
	}

	this <- list()
	this$logical_functions <-
		function () {
			sample(list(
				function () True,
				function () False,
				function () Na), size = 1)[[1]]
		}
	this$recursive_zero <-
		function () {
			sample(list(NULL, list()), size = 1)[[1]]
		}
	this$typed_vector_zero <-
		function () {
			sample(
				list(
					integer(), character(),
					raw(), logical(), numeric()), size = 1)[[1]]
		}
	this$collection_zero <-
		gcombine(
			this$typed_vector_zero, 
			this$recursive_zero)

	this
})

forall <- function (info = "", cases, expect, given, max_time = 0.1) {

	pcall <- sys.call()

	assert(
		all( sapply(cases, is.function) ), pcall,
		lament$non_function_cases(info)
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
	
	# ----- check that the expectation is true for a range of cases

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
				lament$non_boolean_expectation(info, case))

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
			info,
			state$failed_after, 
			state$failed))

	message(info, " passed!")
}
