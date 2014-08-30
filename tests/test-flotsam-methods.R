
kea ::: load_test_dependencies(environment())

message("methods")

	over(val) +

	describe("calling a nullary method works") +
	holdsWhen(
		True,

		x_(val) $ x_I() %is% val
	) +

	run()

	over(val) +

	describe("calling an unevaluated variable works") +
	holdsWhen(
		True,

		x_(val) $ xIs(val) $ x_I(),
		x_(val) $ x_Is(val)

	) +

	describe("self reflection works") +
	holdsWhen(
		True,

		x_(val) $ xIs(self) $ x_I(),
		x_(val) $ x_Is(self)
	) +

	describe("self reflection works") +
	worksWhen(
		is_collection(val),

		x_(val) $ xJoin_(self),
		x_(val) $ xJoin_(c(self, self)),

		x_(xI) $ xMap(list(self)),
		x_(xI) $ xMap(list(self, self, list(self)))
	) +

	run()

