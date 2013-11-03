
message("xRepeat")

forall(
	"repeating the empty list yields the empty list.",
	list(num = G$positive(), coll = G$collection_zero),
	xRepeat(num, coll) %equals% list()
)

forall(
	"repeating a collection is done by end-to-end concatenation.",
	list(num = G$positive(), coll = G$collection()),
	xRepeat(num, coll) %equals% rep(coll, num)
)

message("arrow $ xRepeat")

forall(
	"collection",
	list(),
	x_(coll)$xRepeat(10)
)