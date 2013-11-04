
message("xJuxtapose")

forall(
	"juxtaposing preserves identities within the list",
	list(val = G$integers()),
	xJuxtapose(identity)(val) %equals% list(val)
)

forall(
	"juxtaposing can apply multiple functions",
	list(val = G$integers()),
	xJuxtapose(identity, identity)(val) %equals% list(val, val)
)

forall(
	"juxtaposing works with incrementing",
	G$standard$inc_over_ints(),
	xJuxtapose(fn)(coll) %equals% coll + 1
)

message("arrow $ xJuxtapose")

forall(
	"fn $ xJuxtapose",
	G$standard$inc_over_ints(),
	{
		x_(fn)$xJuxtapose()$x()(coll) %equals% coll + 1
	}
)
