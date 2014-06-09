#!/usr/bin/Rscript --vanilla --slave

require(kiwi)

test_path <- system.file('tests', package = 'kiwi')

if (nchar(test_path) == 0) {
	if ( Sys.getenv()[['USER']] == 'ryan') {
		test_path <- '/home/ryan/Code/kiwi.R/tests'
	}
}

x_( list.files(test_path, full.names = True) ) $
xDo(path := {
	source(path)
	cat('\n')
}) $
xExecute(drop := {
	warnings()
})
