
kea ::: load_test_dependencies(environment())

unit_test("xMedianBy")

	over(nums) +

	it('the median of empty vector is double(0)') +
	holdsWhen(
		suchThat $ is_empty_collection(nums),

		xMedianBy(identity, nums) %is% double(0)
	) +

	it('the median of one number is itself') +
	holdsWhen(
		and_(suchThat $ is_numeric, suchThat $ is_singleton, suchThat $ is_orderable)(nums),

		xMedianBy(identity, nums) == unname(nums)
	) +

	run()

	over(nums) +

	it('fails for na or nan values') +
	failsWhen(
		and_(suchThat $ is_numeric, suchThat $ not_orderable)(nums),

		xMedianBy(identity, nums)
	) +

	run()
