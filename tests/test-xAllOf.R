
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xAllOf (+)")

	over(coll) +

	describe('xAllOf with identity is !any.') +
	when(
		is.logical(coll) && length(coll) > 0,
		xAllOf(identity, coll) %is% length(which(!coll)) == 0
	) +

	describe('partially applying with true is false') +
	when(
		is_collection(coll) && length(coll) > 0,
		xAllOf(function (x) True, coll) == True
	) +

	describe('partially applying with false is true') +
	when(
		is_collection(coll) && length(coll) > 0,
		xAllOf(function (x) False, coll) == False,
		xAllOf(function (x) Na,    coll) == False
	) +

	run()

