
message("xJuxtapose")

forall(
	"juxtaposing preserves identities within the list",
	test_cases$num_positive_integer,
	xJuxtapose(list(identity))(num) %equals% list(num)
)

forall(
	"juxtaposing can apply multiple functions",
	test_cases$num_positive_integer,
	xJuxtapose(list(identity, identity))(num) %equals% list(num, num)
)

forall(
	"juxtaposing works with incrementing",
	test_cases$succ_over_integers,
	xJuxtapose(list(fn))(coll) %equals% list(coll + 1)
)

message("arrow $ xJuxtapose")

forall(
	"collection $ xJuxtapose",
	test_cases$succ_over_integers,
	{
		x_(list(fn))$xJuxtapose()$x()(coll) %equals% coll + 1
	}
)

forall(
	"fn $ xJuxtapose...",
	test_cases$succ_over_integers,
	{
		x_(fn)$xJuxtapose...()$x()(coll) %equals% coll + 1
	}
)
