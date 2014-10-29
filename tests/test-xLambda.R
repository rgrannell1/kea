

kea ::: load_test_dependencies(environment())

message("xLambda")

	over(val) +

	describe("all forms of unary lambda work.") +
	holdsWhen(
		True,
		{
			fn0 <- x := {
				x
			}
			fn1 <- (x) := {
				x
			}

			fn2 <- x := x
			fn3 <- (x) := x
			fn4 <- (...) := {
				..1
			}

			fn0(val) %is% val && fn1(val) %is% val &&
			fn2(val) %is% val && fn3(val) %is% val &&
			fn4(val) %is% val

		}
	) +

	describe("all forms of binary lambdas work.") +
	holdsWhen(
		True,
		{
			fn0 <- (x : y) := {
				list(x, y)
			}
			fn1 <- (x : y) := list(x, y)

			fn2 <- (x : ...) := {
				list(x, ..1)
			}

			fn0(val, val) %is% list(val, val) &&
			fn1(val, val) %is% list(val, val) &&
			fn2(val, val) %is% list(val, val)

		}
	) +

	describe("both the body (for the moment) and environment are identical.") +
	holdsWhen(
		True,
		{

			f <- x := val
			g <- function (x) val

			identical(environment(f), environment(g))
		},
		{

			f <- x := val
			g <- function (x) val

			identical(body(f), body(g))
		}
	) +

	run()

	holdsFor(
		"formals are as expected",

		names( formals((a : b) := {a + b}) ) %is% c('a', 'b'),
		names(formals(x := x))   == 'x',
		names(formals((x) := x)) == 'x'
	)

	failsFor(
		"invalid parametres fail",

		('x') := x,
		('x' : y) := x,
		('x' : 'y') := x,

		(1) := x,
		('x' : 1) := x
	)
