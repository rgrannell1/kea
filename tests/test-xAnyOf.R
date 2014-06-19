
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xAnyOf (+)")

	over(coll) +

	describe('xAnyOf with identity is !any.') +
	when(
		is.logical(coll) && length(coll) > 0,
		xAnyOf(identity, coll) %is% (length(which(coll)) > 0)
	) +

	describe('partially applying with true is false') +
	when(
		is_collection(coll) && length(coll) > 0,
		xAnyOf(function (x) True, coll) == True
	) +

	describe('partially applying with false is true') +
	when(
		is_collection(coll) && length(coll) > 0,
		xAnyOf(function (x) False, coll) == False,
		xAnyOf(function (x) Na,    coll) == False
	) +

	run()
