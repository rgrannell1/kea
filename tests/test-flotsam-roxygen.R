
require(kiwi)

'%+%' <- kiwi ::: '%+%'
throw_kiwi_warning <- kiwi ::: throw_kiwi_warning

# -- This test suite checks to see if each file has the proper roxygen2
# -- documentation



















r_path <- system.file(package = 'kiwi', 'R')
r_path <- '/home/ryan/Code/kiwi.R/R'

if (nchar(r_path) > 0) {

	rdocs <-
		x_(list.files(r_path, full.names = True)) $
		xMap(path := {

			fname <-
				x_(path) $ xExplode('/') $ x_LastOf()

			roxygen <-
				x_(path) $ xRead() $ xToLines() $
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
