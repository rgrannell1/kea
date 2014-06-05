
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xPoll")

	forall(
		"polling the empty collection returns integer(0)",
		test_cases$collection_zero,
		xPoll(Truth, coll) %equals% integer(0))

	forall(
		"polling with truth returns length coll",
		test_cases$collection,
		xPoll(Truth, coll) == length(coll),
		given =
			length(coll) > 0)

	forall(
		"polling with falsity returns 0",
		test_cases$collection,
		xPoll(Falsity, coll) == 0,
		given =
			length(coll) > 0)

	forall(
		"polling with moot returns 0",
		test_cases$collection,
		xPoll(Moot, coll) == 0,
		given =
			length(coll) > 0)

	forall(
		"polling counts true occurrences",
		test_cases$mod2_over_ints,
		xPoll(fn, coll) == length(which(coll %% 2 == 0)),
		given =
			length(coll) > 0)




arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message("xPoll (+)")

	over(coll) +
	describe('xPoll counts true occurrences.') +
	when(
		is.logical(coll) && length(coll) > 0,
		xPoll(identity, coll) %equals% length(which(coll)) == 0
	) +

	describe('xPoll with the empty collection is integer(0)') +
	when(
		is_collection(coll) && length(coll) == 0,
		xPoll(function (x) True, coll)  %equals% integer(0),
		xPoll(function (x) False, coll) %equals% integer(0),
		xPoll(function (x) Na, coll)    %equals% integer(0)
	) +

	describe('xPoll with xFalsity is 0') +
	when(
		is_collection(coll) && length(coll) > 0,
		xPoll(function (x) False, coll) == 0,
		xPoll(function (x) Na, coll)    == 0
	) +

	describe('xPoll with xTruth is length') +
	when(
		is_collection(coll) && length(coll) > 0,
		xPoll(function (x) True, coll) == length(coll)
	) +

	run()
