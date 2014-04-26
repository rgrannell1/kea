
# another hack to build the site; copy site content into staticdocs web.

build_site('/home/ryan/Code/arrow.R', examples = T, hook = function (opts) {

	file.copy(
		'/home/ryan/Code/arrow.R/inst/site-content/',
		'/home/ryan/Code/staticdocs/inst/web/', recursive = TRUE)
})
