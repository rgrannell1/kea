
require(arrow)

'%+%' <- arrow ::: '%+%'
throw_arrow_warning <- arrow ::: throw_arrow_warning

# -- This test suite checks to see if each file has the proper roxygen2
# -- documentation










anyIsMatch <- (rexp : docs) := {
	xAnyOf(xFix_(xIsMatch, rexp), docs)
}









r_path <- system.file(package = 'arrow', 'R')
r_path <- '/home/ryan/Code/arrow.R/R'

if (nchar(r_path) > 0) {

	rdocs <-
		x_(list.files(r_path, full.names = True)) $
		xMap(path := {

			fname <-
				x_(path) $ xExplode('/') $ x_LastOf()

			roxygen <-
				x_(path) $ xReadLines() $
				x_Select(
					xFix_(xIsMatch, "[#][']"))

			list(path, roxygen)
		})

	rdocs $
	xMapply((path : docs) := {

		has_type <- anyIsMatch("@section Type Signature", docs)

		list(path, has_type)
	}) $
	xReject(props := {
		xAllOf(xIdentity, xRestOf(props))
	})



}
