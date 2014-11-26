
kea ::: load_test_dependencies(environment())

message("xLift")

	over(val1, val2) +

	it("%or% has the correct truth table") +
	holdsWhen(
		True,

		(xTruth %or% xTruth)(val1),
		(xTruth %or% xFalsity)(val1),
		(xFalsity %or% xTruth)(val1),
		!(xFalsity %or% xFalsity)(val1)
	) +

	it("%and% has the correct truth table") +
	holdsWhen(
		True,

		(xTruth %and% xTruth)(val1),
		!(xTruth %and% xFalsity)(val1),
		!(xFalsity %and% xTruth)(val1),
		!(xFalsity %and% xFalsity)(val1)
	) +

	it("%or% short-circuits evaluation.") +
	holdsWhen(
		True,

		(xTruth %or% stop)(val1),
		(xTruth %or% stop)(val1, val2)
	) +

	run()




