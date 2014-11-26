
kea ::: load_test_dependencies(environment())

message('xDrop')

	over(num, coll) +

	it("dropping from the empty collection is the empty collection") +
	holdsWhen(
		is_numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xDrop(num, coll) %is% list()
	) +

	it("dropping from the empty collection is the empty collection (named)") +
	holdsWhen(
		is_numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xDrop(num, coll) %is% as_named(list())
	) +

	it("dropping yields the correct collection") +
	holdsWhen(
		is_numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num && num >= 0)) &&
		suchThat $ not_empty_collection(coll),

		{
			ind <- min(length(coll), unlist(num))
			xDrop(num, coll) %is% as.list(tail(coll, -ind))
		}
	) +

	it("drop preserves the names of its remaining elements") +
	holdsWhen(
		is_numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num && num >= 0)) &&
		suchThat $ not_empty_collection(coll),

		as.list( names(xDrop(num, coll)) ) %is% xDrop(num, names(coll))
	) +

	it("take works over all round positive integers") +
	worksWhen(
		is_numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num && num >= 0)) &&
		suchThat $ is_collection(coll),

		xDrop(num, coll)
	) +

	run()
