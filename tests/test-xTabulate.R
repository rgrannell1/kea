
kea ::: load_test_dependencies(environment())

message("xTabulate")

	over(coll) +

	it('tabulating an empty collection is empty list') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xTabulate(coll) %is% list()
	) +

	it('the length of the table is the number of unique elements') +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xTabulate(coll)) == length(unique(coll))
	) +

	it('the first values of the table-tuples is the set of coll') +
	holdsWhen(
		suchThat $ is_collection(coll),

		{
			seta <- lapply( xTabulate(coll), function (x) x[[1]] )
			setb <- as.list(unique(coll))

			length(setdiff(seta, setb)) == 0 && length(setdiff(setb, seta)) == 0
		}
	) +

	it('the first value of the table-tuples is unique') +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(unique(lapply( xTabulate(coll), function (x) x[[1]] ) )) == length(unique(coll))
	) +

	it('tabulating always runs') +
	worksWhen(
		suchThat $ is_collection(coll),

		xTabulate(coll)
	) +

	run()
