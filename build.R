#!/usr/bin/env Rscript

require(docopt, quietly = TRUE, warn.conflicts = FALSE)
require(methods, quietly = TRUE, warn.conflicts = FALSE)




docs <- list()

"
Name:
	build: a hacky build script.
Usage:
	build profile [--previous=<num>] [--time=<time>]
Tasks:
	redocument - roxygenise (roxygenate surely?) the documentation.
	parse      - test that the code parses.
	install    - build the package

Options:
	--previous=<num>     the maximum number of previous releases to examine [default: Inf]
	--time=<time>        the total time to run benchmarks for, in seconds [default: 600]


" -> docs $ master










tasks <- local({

	self <- list()






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

		system( paste('R CMD INSTALL', getwd()) )
		self $ redocument()

	}





	self $ profile <- function (previous, time) {

		profile <- file.path(getwd(), 'inst/profile_all_versions.R')
		system(paste0(profile, ' --previous=', previous, ' --time=', time))

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

}




main(docopt(docs $ master))
