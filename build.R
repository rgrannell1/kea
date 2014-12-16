#!/usr/bin/env Rscript

require(docopt, quietly = TRUE, warn.conflicts = FALSE)
require(methods, quietly = TRUE, warn.conflicts = FALSE)




"
Name:
	build: a hacky build script.
Usage:
	build <task>

" -> doc





tasks <- local({

	self <- list()






	self $ redocument <- function () {

		devtools :: document(roclets = c('rd', 'collate', 'namespace'))

		namespace <- file.path(getwd(), 'NAMESPACE')

		write('useDynLib(kea)',              file = namespace, append = TRUE)
		write('importFrom(Rcpp, sourceCpp)', file = namespace, append = TRUE)

	}

	self $ parse <- function () {

		r_paths <- list.files(getwd(), pattern = '[.]R$|[.]r', recursive = TRUE, full.names = TRUE)

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




	self

})





main <- function (args) {

	task <- args[['task']]

	if ( is.null( tasks[[task]] )) {

		write(paste(task, 'not found.'), stderr())
		quit('no', 1)

	}

	tasks[[task]]()

}




main(docopt(doc))
