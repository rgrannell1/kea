
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xApply')

	forall(
		"applying to an list function identity yields the list",
		test_cases$collection,
		xApply(list, as.list(coll)) %equals% as.list(coll)
	)

	forall(
		"lazy-evaluation doesn't interfere with call evaluation",
		test_cases$num_integer,
		{
			lazy_double <- function (a) {

				double <- function (b) {
					xApply(function (c) 2*c, b)
				}

				double(a)
			}

			lazy_double(num) %equals% (2*num)
		}
	)

message("arrow $ xApply")

message("coll $ xApply")

	forall(
		"coll arrow list identity",
		test_cases$collection,
		x_(coll)$xApply(list)$x() %equals% as.list(coll)
	)

message("fn $ xApply")

	forall(
		"fn arrow list identity",
		test_cases$collection,
		x_(list)$xApply(coll)$x() %equals% as.list(coll)
	)

message("coll $ xApply...")

	forall(
		"coll apply... list identity",
		test_cases$collection,
		x_(coll)$xApply...(list)$x() %equals% list(coll)
	)

message("fn $ xApply...")

	forall(
		"fn apply... list identity",
		test_cases$collection,
		x_(list)$xApply...(coll)$x() %equals% list(coll)
	)
