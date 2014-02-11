
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xAsClosure")

	forall(
		"all primitives may be converted.",
		test_cases$base_primitive,
		{
			closure <- xAsClosure(fn)
			is.function(closure) && !is.primitive(closure)
		}
	)

	forall(
		"parametres are preserveds.",
		test_cases$base_primitive,
		{
			closure <- xAsClosure(fn)
			xFormalsOf(closure) %equals% xFormalsOf(fn)
		}
	)

