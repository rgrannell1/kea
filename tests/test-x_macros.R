
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('Arrow Files')
message('Check that every function has missing declarations')#

arr <- as.environment('package:arrow')

missing_exceptions <- c(
	"xFalsity", "xTruth", "xIrrelevance",
	"xFromChars", "xFromLines", "xFromWords",
	"xLambda", "xList", "xVersion")

fns <-
	x_( ls(arr) ) $
	xSelect (fn_name := {
		grepl('x[A-Z].+[a-z]$', fn_name) && !(fn_name %in% missing_exceptions)
	}) $
	xMap( fn_name := list(fn_name, arr[[fn_name]] ))

fns $ xMapply((fn_name : fn) := {

	body_text <- paste0( deparse(body(fn)), collapse = '\n')

	missing_fn_matchable <-
		x_(xParamsOf(fn)) $
		xSelect(param := param == 'fn' || param == 'pred') $
		x_AnyOf(param := {

			pattern <- paste0(
				"if [(][!]is[.]function[(]", param, "[)]")

			!grepl(pattern, body_text)
		})

	missing_coll <-
		x_(xParamsOf(fn)) $
		xSelect(param := param %in% c('coll', 'colls', 'nums', 'num', 'bools', 'ims', 'raws')) $
		x_AnyOf(param := {

			pattern <- paste0(
				"[!]is[_]atomic[(]", param, "[)]")

			!grepl(pattern, body_text)
		})

	missing_colls <-
		x_(xParamsOf(fn)) $
		xSelect(param := param %in% "colls") $
		x_AnyOf(param := {

			pattern <- "all_elems_are_collection"

			!grepl(pattern, body_text)
		})

	if (xIsTrue(missing_fn_matchable))  {
		stop("no fn match macro detected in ", fn_name)
	}

	if (xIsTrue(missing_coll)) {
		stop("no coll macro detected in ", fn_name)
	}

	if (xIsTrue(missing_colls)) {
		stop("no coll of coll macro detected in ", fn_name)
	}

}) $
xK()
