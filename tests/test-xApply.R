
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

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
