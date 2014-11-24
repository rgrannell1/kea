
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
		suchThat $ is_closure(fn),

		xFormalsOf(fn) %is% as.list(formals(fn))
	) +

	run()
