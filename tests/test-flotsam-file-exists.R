
require(kiwi)

either <- function (...) {
	x__(...) $ xSelect(x. != '') $ x_FirstOf()
}

sep <- .Platform $ file.sep
kiwi_path <- (...) := {

	kiwi <- system.file('', package = 'kiwi')

	gsub(
		xFromChars_(sep, sep), sep,
		xImplode(sep, c(kiwi, ...)) )
}

existing_paths <- (...) := {
	x__(...) $
	xSelect(path := {
		xIsTrue(file.exists(path))
	}) $
	x_AsCharacter()
}





kiwi <- system.file('', package = 'kiwi')

path <- list()
path $ description <- kiwi_path('DESCRIPTION')
path $ namespace   <- kiwi_path('NAMESPACE')
path $ examples    <-
	existing_paths(kiwi_path('inst', 'examples'), kiwi_path('examples'))
path $ tests       <-
	existing_paths(kiwi_path('inst', 'tests'), kiwi_path('tests'))




# -- the basename, path pairs of collated files.
collate_files_ <-
	x_(xRead(path $ description)) $ xToLines()          $
	xDropWhile(
		xNotMatch('Collate'))                           $
	xMap(
		xToChars  %then% xDropWhile(x. != "'") %then%
		xRestOf() %then%
		xTakeWhile(x. != "'") %then% xFromChars)        $
	xMap(
		path := list(base = path, abspath = fp(kiwi, path) ))

# -- arrow functions, and their R path.

namespace_files_ <-
	x_(xRead(path $ namespace)) $ xToLines()      $
	xSelect(xIsMatch('x[A-Z].+[^_][)]'))                   $
	xMap(export := {
		gsub('export[(]|[)]', '', export)
	})                                            $
	xMap(fn_name := {
		list(
			fn_name,
			kiwi_path('R', xFromChars_(fn_name, '.R'))
		)
	})






message('checking that R file has an example file')

	expected_examples_ <-
		namespace_files_ $
		xMapply((base : abspath) := {

			list(
				base,
				xImplode_(
					sep, path $ examples,
					xFromChars_('example-', base, '.R')) )
		})

	expected_examples_ $ xDo( xUnspread((base : path) := {

		if (!file.exists(path)) {
			stop("no example existed for ", base)
		}
	}) )
