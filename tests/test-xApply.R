
message('xApply')

forall(
	"applying to an list function identity yields the list",
	G$standard$coll,
	xApply(list, argscoll) %equals% coll
)

forall(
	"lazy-evaluation doesn't interfere with call evaluation",
	list(a = G$integer(), b = G$integer()),
	{

		f <- function (a) {
			( function (b) xApply( function (c) 2 * c  ) )(a)
		}

		f(a) %equals% (2 * a)
	}
)

message("arrow $ xApply")

forall(
	"coll arrow list identity",
	G$standard$coll,
	x_(coll)$xApply(list) %equals% coll
)

message("fn $ xApply")

forall(
	"fn arrow list identity",
	G$standard$coll,
	x_(list)$xApply(coll) %equals% coll
)
