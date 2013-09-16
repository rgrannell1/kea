	
context("x_ monad laws")

test_that("Type constructor doesn't screw with data (left identity)", {

	expect_equal(x_(10)$x(), 10)
	expect_equal(x_(function() 1)$x()(), 1)

})

test_that("Type constructor flattens nested x_() inputs (right identity)", {

	expect_equal(x_(x_(10))$x(), 10)
	expect_equal(x_(x_(function() 1))$x()(), 1)

})

context("x_ methods")

test_that('type signitures are almost the same between free functions and methods', {

	funcs <- ls('package:arrow', pattern = 'x[A-Z][a-z0-9]+')

	Map(
		function (name) {

			free <- match.fun(name)

			methods <- list(
				coll = 
					x_coll_proto[[name]],
				fn = 
					x_fn_proto[[name]] )

			if (!is.null(methods$coll)) {

				coll_params <- names(formals(methods$coll))				
				param_union <- union( coll_params, names(formals(free)) )

				expect_true(
					length(param_union) <= length(formals(free)),
					label = name)

			}
			if (!is.null(methods$fn)) {
					
				fn_params <- names(formals(methods$fn))
				param_union <- union( fn_params, names(formals(free)) )

				expect_true(
					length(param_union) <= length(formals(free)),
					label = name)
			}
		}, 
		funcs
	)
})

test_that('method chaining works as expected, for some methods.', {

	expect_that(
		x_(function (a, b) a + b)$
		xArity()$
		x(),
		equals(2)
	)

	expect_that(
		x_(list(1, 2, 3))$
		xMap( function (x) x + 1 )$
		x(),
		equals(list(2, 3, 4)) )
 
})























