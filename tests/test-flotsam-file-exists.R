
require(kea)

either <- function (...) {
	x__(...) $ xSelect(x. != '') $ x_FirstOf()
}

sep <- .Platform $ file.sep
kea_path <- (...) := {

	kea <- system.file('', package = 'kea')

	gsub(
		xFromChars_(sep, sep), sep,
		xImplode(sep, c(kea, ...)) )
}

existing_paths <- (...) := {
	x__(...) $
	xSelect(path := {
		xIsTrue(file.exists(path))
	}) $
	x_AsCharacter()
}





kea <- system.file('', package = 'kea')

path <- list()
path $ description <- kea_path('DESCRIPTION')
path $ namespace   <- kea_path('NAMESPACE')
path $ examples    <-
	existing_paths(kea_path('inst', 'examples'), kea_path('examples'))
path $ tests       <-
	existing_paths(kea_path('inst', 'tests'), kea_path('tests'))




# -- the basename, path pairs of collated files.
collate_files_ <-
	x_(xRead(path $ description)) $ xToLines() $
	xDropWhile(
		xNotMatch('Collate')) $
	xMap(
		xToChars  %then% xDropWhile(x. != "'") %then%
		xRestOf() %then%
		xTakeWhile(x. != "'") %then% xFromChars) $
	xMap(
		path := list(base = path, abspath = kea_path(path) ))

# -- arrow functions, and their R path.

namespace_files_ <-
	x_(xRead(path $ namespace)) $ xToLines() $
	xSelect(xIsMatch('x[A-Z].+[^_][)]')) $
	xMap(export := {
		gsub('export[(]|[)]', '', export)
	}) $
	xMap(fn_name := {
		list(
			fn_name,
			kea_path('R', xFromChars_(fn_name, '.R'))
		)
	})






message('checking that R file has an example file')

	expected_examples_ <-
		namespace_files_ $
		xMap( xUnspread((base : abspath) := {

			list(
				base,
				xImplode_(
					sep, path $ examples,
					xFromChars_('example-', base, '.R')) )
		}) )

	expected_examples_ $ xDo( xUnspread((base : path) := {

		if (!file.exists(path)) {
			stop("no example existed for ", base)
		}
	}) )






message('checking that each example is non-empty')

	comment_or_null <-
		xImplode_(
			'|',
			# -- is the line a comment?
			'[ 	]*[#].*$',
			# -- is the line just NULL?
			'^[ 	]*NULL[ 	]*$',
			# -- is the line empty?
			'^$'
		)

	example_lengths_ <-
		expected_examples_ $ xMap( xUnspread((base : abspath) := {

			length_summary <-
				x_(xRead(abspath)) $ xToLines() $
				xReject(
					xIsMatch(comment_or_null)) $
				x_LenOf()

			list(base, length_summary)
		}) )

	empty_examples_ <- example_lengths_ $ xSelect(info := {
		xSecondOf(info) == 0
	})

	if (empty_examples_ $ x_NotEmpty()) {

		message <- xFromChars_(
			'the following ',
				toString(empty_examples_ $ x_LenOf()),

			' examples were empty;\n',
				empty_examples_ $ xMap(xAt(1)) $ x_FromLines())

		warning(message = message)
	}
