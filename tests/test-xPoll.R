
kiwi ::: load_test_dependencies(environment())


message("xPoll")

	over(coll) +

	describe('xPoll counts true occurrences.') +
	holdsWhen(
		is.logical(coll) && length(coll) > 0,
		xPoll(identity, coll) %is% length(which(coll)) == 0
	) +

	describe('xPoll with the empty collection is integer(0)') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xPoll(function (x) True,  coll) %is% integer(0),
		xPoll(function (x) False, coll) %is% integer(0),
		xPoll(function (x) Na,    coll) %is% integer(0)
	) +

	describe('xPoll with xFalsity is 0') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xPoll(function (x) False, coll) == 0,
		xPoll(function (x) Na,    coll) == 0
	) +

	describe('xPoll with xTruth is length') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xPoll(function (x) True, coll) == length(coll)
	) +

	run()

message("xPoll")

	over(coll) +

	describe('xPoll fails for non-collections.') +
	failsWhen(
		!is_collection(coll),
		xPoll(function (x) True,  coll),
		xPoll(function (x) False, coll),
		xPoll(function (x) Na,    coll)
	) +

	run()
