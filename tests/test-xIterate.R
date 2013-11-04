
message("xIterate")

forall(
	"Return is the identity function",
	list(init = G$integers()),
	xIterate(function (n) Return(n), init) %equals% init
)

forall(
	"incrementing to a value works",
	list(upper = G$positive()),
	xIterate(
		function (n) {
			if (n == upper) Return(n) else n + 1
		},
		0) %equals% upper
)

message("arrow $ xIterate")

forall(
	"function $ xIterate",
	list(upper = G$positive()),

	x_( function (n) if (n == upper) Return(n) else n + 1)$
	xIterate(0) %equals% upper
)

