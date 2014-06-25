
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xAt")

	forall(
		"indexing a letter with xAt selects the letter",
		test_cases$letters_and_index,
		xAt(num, coll) == coll[[num]]
	)



message('xAt (-)')

	over(coll) +

	describe('fails when index is too large') +
	failswhen(
		is_collection(coll) && length(coll) >= 1,
		xAt(length(coll) + 1, coll)
	) +

	describe('fails when index is too large') +
	failswhen(
		is_collection(coll) && length(coll) >= 1,
		xAt(0, coll)
	) +

	run()
