#!/usr/bin/env Rscript

require(docopt, quietly = TRUE, warn.conflicts = FALSE)
require(methods, quietly = TRUE, warn.conflicts = FALSE)




docs <- list()

"
Name:
    build: a hacky build script.
Usage:
    build profile [--previous=<num>] [--time=<time>]
    build redocument
    build install
    build parse
    build test
Tasks:

    redocument - roxygenise (roxygenate surely?) the documentation. Currently
                 fixes an issue I'm having where Rcpp's dependencies is stripped from the NS on rebuild.

    parse      - test that the code parses. Faster than rebuilding.

    install    - build the package and redocument.

Options:
    --previous=<num>     the maximum number of previous releases to examine [default: Inf]
    --time=<time>        the total time to run benchmarks for, in seconds [default: 600]

" -> docs $ master





tasks <- local({

	self <- list()

	sh <- function (...) {
		system(paste0(...))
	}





	self $ redocument <- function () {

		devtools :: document(roclets = c('rd', 'collate', 'namespace'))

		namespace <- file.path(getwd(), 'NAMESPACE')

		write('useDynLib(kea)',              file = namespace, append = TRUE)
		write('importFrom(Rcpp, sourceCpp)', file = namespace, append = TRUE)

	}





	self $ parse <- function () {

		r_paths <- list.files(getwd(),
			pattern    = '[.]R$|[.]r',
			recursive  = TRUE,
			full.names = TRUE
		)

		sapply(r_paths, function (fpath) {

			tryCatch(
				parse(fpath, keep.source = FALSE),
				error = function (err) {
					write(paste('could not parse', fpath), stderr())
				}
			)

		})

		invisible(NULL)

	}





	self $ install <- function () {

		sh('R CMD INSTALL ', getwd())
		self $ redocument()

	}





	self $ profile <- function (previous, time) {

		profile <- file.path(getwd(), 'inst/profile_all_versions.R')
		sh(profile, ' --previous=', previous, ' --time=', time)

	}




	self $ test <- function () {
		devtools::check(document = FALSE)
	}





	self

})





main <- function (args) {

	if (args $ redocument) {
		tasks $ redocument()
	}

	if (args $ parse) {
		tasks $ parse()
	}

	if (args $ install) {
		tasks $ install()
	}

	if (args $ profile) {
		tasks $ profile(args $ `--previous`, args $ `--time`)
	}

	if (args $ test) {
		tasks $ test()
	}

}




main(docopt(docs $ master))
