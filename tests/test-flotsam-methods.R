
kea ::: load_test_dependencies()

message("methods")

	over(val) +

	describe("universal methods are available to all data types") +
	holdsWhen(
		True,
		x_(val) $ x_I() %is% val
	) +

	over(val) +

	describe("calling an unevaluated variable works") +
	holdsWhen(
		True,

		x_(val) $ xIs(val) $ x_I(),
		x_(val) $ x_Is(val)

	) +

	run()

	over(val1, val2, val3) +

	describe("variadic functions work") +
	holdsWhen(
		True,

		identical( x_(val1) $ x_FirstOf_(val2, val3), val1 )
	) +

	run()

	over(val) +

	describe("lexical closure works") +
	worksWhen(
		True,
		{

			f <- function (x) {
				x_(x) $ x_Tap(function (y) x)
			}

			f(val)
		}
	) +

	run()
