
require (arrow)
require (needy)

opts <- list(
	target_dir = 
		'/home/rgrannell1/Desktop/packages'
)

#------------------ CRAN data.

cran_mirror <- ( function () {
	# return a function that returns a random cran mirror.

	cran_mirrors <- getCRANmirrors()[,'URL']
	function () {
		sample(cran_mirrors, size = 1)
	}
} )()

cran_raw_data <- utils::available.packages()
cran_data <- xZipWith(
	xAsVariadic(row := {

		list(
			package = row[[1]], depends = row[[2]],
			imports = row[[3]], license = row[[4]],
			suggests = row[[5]], enhances = row[[6]],
			linking_to = row[[7]] )

	}),
	cran_raw_data[,'Package'],
	cran_raw_data[,'Depends'],
	cran_raw_data[,'Imports'],
	cran_raw_data[,'License'],
	cran_raw_data[,'Suggests'],
	cran_raw_data[,'Enhances'],
	cran_raw_data[,'LinkingTo']
)

#------------------ Accessory functions used below.

select_field <- field := {

	x_(cran_data)$
	xMap(row := row[[field]] )$
	xReject(xIsNa)$
	x()

}

grab_package <- package := {
	# download each package as a tarball.

	try(download.packages(
		package, repos = cran_mirror(),
		destdir = opts$target_dir), silent = True)
}

is_targz <- path := {
	grepl('[.]tar[.]gz$', path)
}

untarball <- path := {
	wd <- getwd()
	setwd(opts$target_dir)
		untar(path)
	setwd(wd)
}

#------------------ Act I: Grab All R code on CRAN.

#--- grab each package

x_(select_field('package'))$xMap(grab_package)

#--- grab the .R files in each package

files <- list.files(
	opts$target_dir,
	full.names = True
)
tarballs <- xSelect(is_targz, files)
x_( tarballs )$xDo(untarball)

files <- list.files(
	opts$target_dir,
	full.names = True
)
folders <- xReject(is_targz, files)

# recurse into each folder, grabbing R files.

r_package <- x_(folders)$xDo(path := {

	list.files(
		path,
		pattern = '[.]R$|[.]r$', 
		full.names = True,
		recursive = True)

})$x()

r_source <- x_(r_package)$xMap(package := {
	
	x_(package)$xMap(path := {
		tryCatch(readLines(path), error = function (error) "")
	})$x()

})$x()

names(r_source) <- x_(folders)$xMap(path := {
	xLast( xSplitStr("/", path) )
})$x()


#------------------ Act II: Generate a small feature space.

is_roxygenated <- source := {
	# does a package use roxygen2 documentation?
	# can be done by checking dependencies, but this is a better example.

	roxygen_pattern <- xCollapse('|',
			c('[@]exports', '[@]rdname', '[@]title',
			'[@]usage', '[@]name', '[@]docType', '[@]format',
			'[@]source', '[@]exportClass', '[@]exportMethod',
			'[@]S3method', '[@]import','[@]importFrom',
			'[@]importClassesFrom', '[@]importMethodsFrom',
			'[@]useDynLib', '[@]inheritParams', '[@]method',
			'[@]examples', '[@]example', '[@]return', '[@]author',
			'[@]note', '[@]section', '[@]keywords', '[@]aliases',
			'[@]concepts', '[@]references', '[@]seealso', '[@]family',
			'[@]template', '[@]templateVar'))

	xAny(
		line := {
			any( grepl(roxygen_pattern, line) )
		},
		source
	)
}

test_framework <- package := {

	package_data <- xFlatten(1, xSelect(
		row := {
			row$package == package
		},
		cran_data
	))

	linked <- x_(c(
		package_data$suggests,
		package_data$imports,
		package_data$depends))$
	xReject(xIsNa)$
	xCollapse(" ")$
	xSplitStr(", ")$
	x()

	intersect(linked, c('testthat', 'RUnit', 'assertive', 'needy'))
}

features <- x_(names(r_source))$xMap(package := {
	# generate feature vectors.

	source <- r_source[[package]]

	list(
		roxygenated = 
			xAny(xIdentity, x_(source)$xMap(is_roxygenated)$x()),
		nlines = 
			length(unlist(source)),
		test_package = 
			test_framework(package))

})$x()
