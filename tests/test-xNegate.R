
kea ::: load_test_dependencies(environment())

unit_test('xNegate')

	over(bool, val) +

	it("negate negates") +
	holdsWhen(
		is.logical(bool) && length(bool) == 1,

		xNegate(function (x) bool)(val) %is% !(function(x) bool)(val)
	) +

	run()
