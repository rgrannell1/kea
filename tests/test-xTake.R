
kea ::: load_test_dependencies(environment())

unit_test('xTake')

	over(num, coll) +

	it("taking from the empty collection is the empty collection") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xTake(num, coll) %is% list()
	) +

	it("taking from the empty collection is the empty collection") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xTake(num, coll) %is% as_named(list())
	) +

	it("taking yields the correct collection") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		suchThat $ not_empty_collection(coll),

		{
			ind <- min(length(coll), unlist(num))
			xTake(num, coll) %is% as.list(head(coll, ind))
		}
	) +

	it("take works over all round positive integers") +
	worksWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		suchThat $ is_collection(coll),

		xTake(num, coll)
	) +

	it("take preserves names") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		suchThat $ is_collection(coll),

		names(xTake(num, coll)) %is% names(coll[ seq_len(min( unlist(num), length(coll) )) ])
	) +

	run()
