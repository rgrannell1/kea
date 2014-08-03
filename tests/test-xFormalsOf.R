
kea ::: load_test_dependencies(environment())

message('xFormalsOf')

	over(fn) +

	describe("nullary yields empty list") +
	holdsWhen(
		True,

		xFormalsOf(function () {}) %is% list()
	) +

	describe("nullary yields empty list") +
	holdsWhen(
		is.function(fn) && !is.primitive(fn),

		xFormalsOf(fn) %is% as.list(formals(fn))
	) +

	run()
