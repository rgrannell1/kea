
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message("xNoneOf (+)")

	over(coll) +

	describe('xNoneOf with identity is !any.') +
	when(
		is.logical(coll) && length(coll) > 0,
		xNoneOf(identity, coll) %equals% length(which(coll)) == 0
	) +

	describe('partially applying with true is false') +
	when(
		is_collection(coll) && length(coll) > 0,
		xNoneOf(function (x) True, coll) == False
	) +

	describe('partially applying with false is true') +
	when(
		is_collection(coll) && length(coll) > 0,
		xNoneOf(function (x) False, coll) == True,
		xNoneOf(function (x) Na,    coll) == True
	) +

	run()

