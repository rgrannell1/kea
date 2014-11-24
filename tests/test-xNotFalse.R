
kea ::: load_test_dependencies(environment())

message("xNotFalse")

	over(val) +

	describe('xNotFalse of false is false.') +
	holdsWhen(
		suchThat $ is_false(val),

		!xNotFalse(val)
	) +

	describe('xNotFalse of values is true.') +
	holdsWhen(
		suchThat $ not_false(val),

		xNotFalse(val)
	) +

	describe('xNotFalse is always true or false') +
	holdsWhen(
		TRUE,

		xNotFalse(val) || !xNotFalse(val)
	) +

	run()
