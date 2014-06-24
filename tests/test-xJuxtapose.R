
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

require(kiwi)

message("xJuxtapose")

	over(num, val) +

	describe("juxataposing with list(identity) is list(val)") +
	when(
		True,
		xJuxtapose_(identity)(val) %is% list(val)
	) +

	describe("juxtaposing with many identities is a list of vals") +
	when(
		is.numeric(num) && is.finite(num) && length(num) == 1 && num > 0 && num < 10000,
		xJuxtapose(rep(list(identity), num))(val) %is% rep(list(val))
	) +

	run()
