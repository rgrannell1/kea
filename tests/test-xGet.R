
message("xGet")

forall("getting from list with two identical keys returns both.",
	list(key = G$word()),
	{
		xGet(key)(list()) %equals% list()
	}
)

forall("getting from list with two identical keys returns both.",
	list(key = G$word()),
	{
		coll <- structure(list(1, 2), names = c(key, key))
		xGet(key)(coll) %equals% list(1, 2)
	}
)

forall("only correct matches are returned.",
	list(key = G$word()),
	{
		coll <- structure(
			list(1, 2, 3),
			names = c(key, paste(key, '2'), key))

		xGet(key)(coll) %equals% list(1, 3)
	}
)
