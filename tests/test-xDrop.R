
kiwi ::: load_test_dependencies(environment())


message('xDrop')

	over(num, coll) +

	describe("dropping from the empty collection is the empty collection") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && num > 0 && round(num) == num)) &&
		is_collection(coll) && length(coll) == 0,
		xDrop(num, coll) %is% list()
	) +

	describe("dropping yields the correct collection") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && num > 0 && round(num) == num)) &&
		is_collection(coll) && length(coll) > 0,
		{
			ind <- min(length(coll), num)
			xDrop(num, coll) %is% as.list(tail(coll, -ind))
		}
	) +

	describe("take works over all round positive integers") +
	worksWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && num > 0 && round(num) == num)) &&
		is_collection(coll),
		xDrop(num, coll)
	) +

	run()
