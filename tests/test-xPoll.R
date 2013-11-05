
message("xPoll")

forall(
	"polling the empty collection returns 0",
	list(coll = G$collection_zero),
	xPoll(Truth, coll) == 0)

forall(
	"polling with truth returns length coll",
	list(coll = G$collection()),
	xPoll(Truth, coll) == length(coll),
	given =
		length(coll) > 0)

forall(
	"polling with falsity returns 0",
	list(coll = G$collection()),
	xPoll(Falsity, coll) == 0,
	given =
		length(coll) > 0)

forall(
	"polling with moot returns 0",
	list(coll = G$collection()),
	xPoll(Moot, coll) == 0,
	given =
		length(coll) > 0)

forall(
	"polling counts true occurrences",
	G$standard$mod2_over_ints(),
	xPoll(fn, coll) == length(which(coll %% 2 == 0)) )

message("arrow $ xPoll")

forall(
	"fn $ xPoll",
	G$standard$mod2_over_ints(),
	x_(fn)$xPoll(coll)$x() == length(which(coll %% 2 == 0))
)

forall(
	"coll $ xPoll",
	G$standard$mod2_over_ints(),
	x_(coll)$xPoll(fn)$x() == length(which(coll %% 2 == 0))
)
