
forall <- arrow:::forall
assert <- arrow:::assert
test_cases <- arrow:::test_cases

write_error <- arrow:::write_error

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
			xPartial...(xNotMember, coll = tests),
			r_files
		)

	nonexisting <-
		xSelect(
			xPartial...(xNotMember, coll = r_files),
			tests
		)

	if (xNotEmpty(untested)) {
		write_error( "untested functions: ", deparse(xAsCharacter(untested)) )
	}

	if (xNotEmpty(nonexisting)) {
		write_error( "untested functions: ", deparse(xAsCharacter(nonexisting)) )
	}

