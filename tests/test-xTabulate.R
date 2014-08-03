
kea ::: load_test_dependencies(environment())

message("xTabulate")

	over(coll) +

	describe('tabulating an empty collection is empty list') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xTabulate(coll) %is% list()
	) +

	describe('the length of the table is the number of unique elements') +
	holdsWhen(
		is_collection(coll),
		length(xTabulate(coll)) == length(unique(coll))
	) +

	describe('the first values of the table-tuples is the set of coll') +
	holdsWhen(
		is_collection(coll),
		{
			seta <- lapply( xTabulate(coll), function (x) x[[1]] )
			setb <- as.list(unique(coll))

			length(setdiff(seta, setb)) == 0 && length(setdiff(setb, seta)) == 0
		}
	) +

	describe('the first value of the table-tuples is unique') +
	holdsWhen(
		is_collection(coll),
		length(unique(lapply( xTabulate(coll), function (x) x[[1]] ) )) == length(unique(coll))
	) +

	describe('tabulating always runs') +
	worksWhen(
		is_collection(coll),
		xTabulate(coll)
	) +

	run()
