
require(arrow)

'%+%' <- arrow ::: '%+%'
throw_arrow_error <- arrow ::: throw_arrow_error

# -- This series of unit tests checks that files that
# -- should exist, do exist.





r_paths <- x_(list(
	R =
		system.file(package = 'arrow', 'R'),
	examples =
		system.file(package = 'arrow', 'inst/examples'),
	tests =
		system.file(package = 'arrow', 'tests'),
	man =
		system.file(package = 'arrow', 'man'),
	namespace =
		system.file(package = 'arrow', 'NAMESPACE')
))

# 1. Regular expression patterns.

non_variadic_pattern <- 'x[A-Z][a-zA-Z]+'

# 2. Convert function name to path name.

as_r_path <- function (fnname) {
	xFromChars_(r_paths $ x_AtKey('R'), '/', fnname, '.R')
}

as_example_path <- function (fnname) {
	xFromChars_(r_paths $ x_AtKey('examples'), '/', fnname, '.R')
}

as_test_path <- function (fnname) {
	xFromChars_(r_paths $ x_AtKey('tests'), '/', fnname, '.R')
}














if (nchar(r_paths $ x_AtKey('tests')) > 0) {

	variadic_exports <-
		r_paths $ xAtKey('namespace') $ xReadLines() $
		xSelect(export := {
			xIsMatch(
				# -- match any non-variadic exports.
				xFromChars_('export[(]', non_variadic_pattern, '[)]'),
				export)
		}) $
		xMap(export := {
			# -- positionally remove the export tag.
			x_(export) $ xToChars() $ xDrop(7) $ xInitOf() $ x_FromChars()
		})






	check_for_missing_exports <- (as_path : dir_message : exceptions) := {
		# -- complain about variadic_exports missing from a file.

		not_found <-
			variadic_exports $ xReject(export := {
				# -- remove the functions that do have matching files.
				xIsTrue( file.exists(as_r_path(export)) )
			}) $
			xReject(export := {
				# -- these functions are sanctioned to be missing their own file.
				xIsMember_(export, exceptions)
			})

		if (not_found $ x_NotEmpty()){

			message <- xFromChars_(
				"the following functions were missing their own ", dir_message, " files:",
				not_found $ x_Implode(', '))

			throw_arrow_error(message = message)
		}
	}

	message(
		'check that every exported function ' %+%
		'Fun has a corresponding xFun.R file')

		check_for_missing_exports(as_r_path, 'arrow/R',
			c('xVectorize'))

	message(
		'check that every exported function ' %+%
		'Fun has a corresponding example-xFun.R file')

		check_for_missing_exports(as_example_path, 'arrow/inst/examples',
			c('xVectorize'))

	message(
		'check that every exported function ' %+%
		'Fun has a corresponding test-xFun.R file')

		check_for_missing_exports(as_test_path, 'arrow/inst/tests',
			c('xVectorize'))

}