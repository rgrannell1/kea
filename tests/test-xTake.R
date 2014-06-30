
kiwi ::: load_test_dependencies(environment())


message('xTake')

	over(num, coll) +

	describe("taking from the empty collection is the empty collection") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		xTake(num, coll) %is% list()
	) +

	describe("taking from the empty collection is the empty collection") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		xTake(num, coll) %is% as_named(list())
	) +

	describe("taking yields the correct collection") +
	holdsWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll) && length(coll) > 0,
		{
			ind <- min(length(coll), num)
			xTake(num, coll) %is% as.list(head(coll, ind))
		}
	) +

	describe("take works over all round positive integers") +
	worksWhen(
		is.numeric(num) && !is.na(num) &&
		(length(num) == 0 || (length(num) == 1 && round(num) == num & num >= 0)) &&
		is_collection(coll),
		xTake(num, coll)
	) +

	run()
