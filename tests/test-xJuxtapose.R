
kea ::: load_test_dependencies(environment())

unit_test("xJuxtapose")

	over(num, val) +

	it("juxataposing with list(identity) is list(val)") +
	holdsWhen(
		True,

		xJuxtapose_(identity)(val) %is% list(val)
	) +

	it("juxtaposing with many identities is a list of vals") +
	holdsWhen(
		is.numeric(num) && is.finite(num) && length(num) == 1 && num > 0 && num < 1000 && round(num) == num,

		xJuxtapose(rep(list(identity), num))(val) %is% rep(list(val))
	) +

	run()
