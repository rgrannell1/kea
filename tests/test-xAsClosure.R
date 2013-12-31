
forall <- arrow:::forall
test_cases <- arrow:::test_cases

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
			xFormals(closure) %equals% xFormals(fn)
		}
	)

message("arrow $ xAsClosure")

message("function $ xAsClosure")

	forall(
		"all primitives may be converted.",
		test_cases$base_primitive,
		{
			is.function(x_(fn)$xAsClosure()$x_()) &&
			!is.primitive( x_(fn)$xAsClosure()$x_())
		}
	)

message("function $ x_AsClosure")

	forall(
		"all primitives may be converted.",
		test_cases$base_primitive,
		{
			is.function(x_(fn)$x_AsClosure()) &&
			!is.primitive( x_(fn)$x_AsClosure())
		}
	)
