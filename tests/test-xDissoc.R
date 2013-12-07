
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xDissoc")

forall(
	"the empty collection always yields the empty list.",
	list(coll = G$collection_zero),
	xDissoc(coll) %equals% list()
)

forall(
	"disassociating names from a list works",
	list(coll = test_cases$collection, key = G$word()),
	{

		names(coll) <- rep(key, length(col))

		dissociated <- lapply(
			seq_along(coll),
			function (ind) {
				list( names(coll)[[ind]], coll[[ind]] )
			}
		)

		xDissoc(coll) %equals% dissociated
	},
	given = length(coll) > 0
)

message("arrow $ xDissoc")

