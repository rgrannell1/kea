
kiwi ::: load_test_dependencies(environment())


message('xDrop')

	over(num, coll) +

	describe("dropping from the empty collection is the empty collection") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		xDrop(num, coll) %is% list()
	) +

	describe("dropping from the empty collection is the empty collection (named)") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		xDrop(num, coll) %is% as_named(list())
	) +

	describe("dropping yields the correct collection") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll) && length(coll) > 0,
		{
			ind <- min(length(coll), num)
			xDrop(num, coll) %is% as.list(tail(coll, -ind))
		}
	) +

	describe("take works over all round positive integers") +
	worksWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll),
		xDrop(num, coll)
	) +

	run()
