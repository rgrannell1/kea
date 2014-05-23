
require(arrow)

'%+%' <- arrow ::: '%+%'
throw_arrow_warning <- arrow ::: throw_arrow_warning

# -- This test suite checks to see if each file has the proper roxygen2
# -- documentation










validate_docs <- docs := {
	# TODO
}









r_path <- '/home/ryan/Code/arrow.R/R'
r_path <- system.file(package = 'arrow', 'R')

if (nchar(r_path) > 0) {

	rdocs <-
		x_(list.files(r_path, full.names = True)) $
		xMap(path := {

			fname <-
				x_(path) $ xExplode('/') $ x_LastOf()

			# -- how many non-empty lines are there?
			roxygen <-
				x_(path) $ xReadLines() $
				x_Select(
					xFix_(xIsMatch, "[#][']"))

			list(path, roxygen)
		})
}
