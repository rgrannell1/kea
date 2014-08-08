#!/usr/bin/Rscript --vanilla --slave

require(kea)

fn_name <- commandArgs(trailingOnly = TRUE)

test_path <- system.file('tests', package = 'kea')

if (nchar(test_path) == 0) {
	if ( Sys.getenv()[['USER']] == 'ryan') {
		test_path <- '/home/ryan/Code/kea.R/tests'
	}
}

x_( list.files(test_path, full.names = True) ) $
xSelect(xIsMatch( paste0('test-', fn_name, '[.]R') )) $
xDo(path := {
	source(path)
	cat('\n')
}) $
xExecute(drop := {
	warnings()
})
