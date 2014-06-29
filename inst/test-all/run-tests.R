#!/usr/bin/Rscript --vanilla --slave

require(kiwi)

# Code is very similar to that in Hadley Wickham's testthat, but
# I'd rather not take a dependency for a single function.

test_path     <- system.file('tests', package = 'kiwi')
property_path <- system.file(test_path, 'property-tests')

test_dir <- function (path) {

	test_files <- list.files(path, full.names = True, pattern = 'test-.+[.][rR]')

	lapply(test_files, source)
}

test_dir(property_path)
