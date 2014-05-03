
forall <- arrow:::forall
assert <- arrow:::assert
test_cases <- arrow:::test_cases

write_error <- arrow:::write_error
wail <- arrow:::wail

require(arrow)

message('Arrow Files')
message('Check that test files exist')

	r_files <-
		x_( list.files('/home/ryan/Code/arrow.R/R') ) $
		xSelect(
			filename := {
				grepl('^x[A-Z][a-z]+[.][R]', filename)
			}) $
		x_Map(
			filename := {
				gsub('[.]R', '', filename)
			})

	tests <-
		x_( list.files('/home/ryan/Code/arrow.R/tests') ) $
		xSelect(
			filename := {
				grepl('test-x[A-Z][a-z]+[.][R]', filename)
			}) $
		xMap(
			filename := {
				gsub('[.]R', '', filename)
			}) $
		x_Map(
			filename := {
				gsub('test[-]', '', filename)
			})

	untested <-
		xSelect(
			xFix_(xNotMember, coll = tests),
			r_files
		)

	nonexisting <-
		xSelect(
			xFix_(xNotMember, coll = r_files),
			tests
		)

	if (xNotEmpty(untested)) {
		write_error( "untested functions: ", deparse(xAsCharacter(untested)) )
	}

	if (xNotEmpty(nonexisting)) {
		write_error( "untested functions: ", deparse(xAsCharacter(nonexisting)) )
	}

message('check that there are sufficient examples')

	x_(list.files('/home/ryan/Code/arrow.R/inst/examples', full.names = True)) $
	xTap(examples := {

		too_short <-
			x_(examples) $ xMap(example := {

				x_(example) $ xReadLines() $ xReject(str := grepl('[ 	\n]+', str)) $ x_LenOf()


			}) $
			xSortBy(xI) $
			xAddKeys(examples) $ xUnzipKeys() $
			x_Select(xUnspread( (key : val) := val < 10 ))

		too_short_names <-
			x_(too_short) $ xMap(xFirstOf) $ x_Map(path := xLastOf(xExplode('/example-', path)) )

		if (xNotEmpty(too_short_names)) {
			warning(
				xLenOf(too_short_names), " some functions had very few examples: ", xFromWords(too_short_names))
		}

		invisible(Null)
	})









message('check that there is a type signature for every file')

	missing_types <-
		x_(list.files('/home/ryan/Code/arrow.R/R', full.names = True)) $
		xSelect(path := grepl('x[A-Z].+', path)) $
		xMap(path := c(path, path)) $ xZipKeys() $
		xMap(xReadLines) $ xMap(file := {

			x_(file) $ x_Any(line := grepl("Type Signature", line))

		}) $
		# -- get the paths that don't have examples and should.
		xReject(xI) $ xUnzipKeys() $ xMap(xFirstOf) $ xMap( path := xLastOf(xExplode('/', path)) )

	if (!missing_types $ x_IsSubset_("xLambda.R", "xList.R", "")) {
		warning("type signatures missing from ", missing_types $ x_Implode(", "))
	}

message('check that the start of each arrow function is the functions file name')

	comments <-
		x_(list.files('/home/ryan/Code/arrow.R/R', full.names = True)) $
		xSelect(path := grepl('x[A-Z].+', path)) $
		xMap(path := c(path, path)) $ xZipKeys() $
		xMap(xReadLines) $
		xMap(line := xSelect(line := grepl("#'", line), line) )

	comments $ xUnzipKeys() $ xMap( xUnspread( (path : lines) := {

		function_name <- x_(path) $ xExplode('/') $ xLastOf() $ xExplode('[.]R') $ x_FirstOf()

		x_(lines) $ xMap(line := {
			grepl(xFromChars_("#'[ 	]+", function_name), line)
		}) $
		xAny(xI) $ x_Join_(function_name)

	}) ) $
	xSelect(xFirstOf)

