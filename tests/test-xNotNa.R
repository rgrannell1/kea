
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xNotNan')

	forall(
		"a vector of nan's is nan",
		test_cases$nans,
		!any(xNotNan(coll))
	)

message('arrow $ xNotNan')

	forall(
		"collection $ xNotNan",
		test_cases$nans,
		!any(x_(coll)$xNotNan()$x_())
	)

message('arrow $ x_NotNan')

	forall(
		"collection $ x_NotNan",
		test_cases$nans,
		!any(x_(coll)$x_NotNan())
	)
