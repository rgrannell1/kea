
forall <- arrow:::forall
test_cases <- arrow:::test_cases

write_error <- arrow:::write_error

require(arrow)

# this code needs rewriting: there are new methods that would make it clearer.

message('check that there is a type signature for every file')

	missing_types <-
		x_(list.files('/home/ryan/Code/arrow.R/R', full.names = True)) $
		xSelect(path := grepl('x[A-Z].+', path)) $
		xMap(path := c(path, path)) $ xZipKeys() $
		xMap(xReadLines) $ xMap(file := {

			x_(file) $ x_AnyOf(line := grepl("Type Signature", line))

		}) $
		# -- get the paths that don't have examples and should.
		xReject(xI) $ xUnzipKeys() $ xMap(xFirstOf) $ xMap( path := xLastOf(xExplode('/', path)) )

	if (!missing_types $ x_IsSubset_("xLambda.R", "xList.R", "")) {
		warning("type signatures missing from ", missing_types $ x_Implode(", "))
	}
