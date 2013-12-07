
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xExists")

forall(
	"xExists of truth is true`",
	list(coll = test_cases$collection),
	xExists(Truth, coll, coll),
	given =
		length(coll) > 0
)

forall(
	"xExists of falsity is false",
	list(coll = test_cases$collection),
	!xExists(Falsity, coll, coll),
	given =
		length(coll) > 0
)

forall(
	"xExists of moot is false",
	list(coll = test_cases$collection),
	!xExists(Moot, coll, coll),
	given =
		length(coll) > 0
)

# might occasionally fail!

forall(
	"xExists of a function that sometimes yields true is true",
	list(ints = G$integers()),
	xExists(
		function (a, b) {
			a %% 2 == 0 || b %% 2 == 1
		}, ints, ints),
	given =
		length(ints) > 0
)

message("arrow $ xExists")

forall(
	"fn $ xExists",
	list(coll = test_cases$collection),
	x_(Truth)$xExists(coll, coll)$x(),
	given =
		length(coll) > 0
)

forall(
	"coll $ xExists",
	list(coll = test_cases$collection),
	x_(coll)$xExists(Truth)$x(),
	given =
		length(coll) > 0
)
