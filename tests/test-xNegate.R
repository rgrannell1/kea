
kea ::: load_test_dependencies()

message('xNegate')

	over(bool, val) +

	describe("negate negates") +
	holdsWhen(
		is.logical(bool) && length(bool) == 1,

		xNegate(function (x) bool)(val) %is% !(function(x) bool)(val)
	) +

	run()
