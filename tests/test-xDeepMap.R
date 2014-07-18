
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xDeepMap")

	over(coll) +

	describe('deepmapping over empty list is empty list') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xDeepMap(identity, coll) %is% list()
	) +

	run()
