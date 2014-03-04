
insist <- arrow:::insist
forall <- arrow:::forall
test_cases <- arrow:::test_cases
stringify_call <- arrow:::stringify_call

invoking_call <- stringify_call(
	quote(xMap(x := x^2, coll = 1:10)) )

message("insist $ max_must_be_less_than_length_of")

message("insist $ minimum_must_be_greater_than")

message("insist $ must_be_atom")

	forall(
		"must be atom works",
		test_cases$collection,
		insist $ must_be_atom(coll, invoking_call),
		given =
			length(coll) == 1
	)

message("insist $ must_be_logical_result")

	test_sym <- NULL

	forall(
		"must_be_logical_result works for logicals.",
		test_cases$logical,
		insist $ must_be_logical_result(
			coll, test_sym, invoking_call)
	)
message("insist $ must_be_collection")


	forall(
		"must be must_be_character works",
		test_cases$collection,
		insist $ must_be_collection(coll, invoking_call)
	)

message("insist $ must_be_collection_of_collections")

message("insist $ must_be_collection_of_equal_length")

message("insist $ must_be_collection_of_fn_matchable")

message("insist $ must_be_collection_of_lengths")

message("insist $ must_be_collections_of_length_matching")

message("insist $ must_be_equal_length")

message("insist $ must_be_fn_matchable")

message("insist $ must_be_fully_named")

message("insist $ must_be_greater_than")

message("insist $ must_be_grequal_than")

message("insist $ must_be_longer_or_equal_than")

	forall(
		"must be longer than or equal than works when longer than",
		test_cases$collection,
		insist $ must_be_longer_or_equal_than(coll, length(coll), invoking_call)
	)

message("insist $ must_be_longer_than")

	forall(
		"must be longer than works when longer than",
		test_cases$collection,
		insist $ must_be_longer_than(coll, length(coll) - 1, invoking_call)
	)

message("insist $ must_be_non_primitive")

	forall(
	    "must be non primitive works for non primitives",
	    test_cases$base_function,
	    insist $ must_be_non_primitive(fn, invoking_call)
	)

message("insist $ must_be_nonnegative")

	forall(
		"must be non negative works for nonnegative numbers",
		test_cases$positive_integers,
		insist $ must_be_nonnegative(coll, invoking_call)
	)

message("insist $ must_be_of_length")

	forall(
		"must be of length works for a single length",
		test_cases$collection,
		insist $ must_be_of_length(coll, length(coll), invoking_call)
	)

	forall(
		"must be of length works for multiple lengths",
		test_cases$collection,
		{
			size <- sample(1:10, size = 1)
			coll <- coll[1:size]
			insist $ must_be_of_length(coll, 1:10, invoking_call)
		},
		given =
			length(coll) > 0
	)

message("insist $ must_be_parametres_of")

	local({

		fn <- function (a, b, c) {
			a + b + c
		}

		insist $ must_be_parametres_of(
			c('a', 'b'), fn, invoking_call)

	})

message("insist $ must_be_unlocked")

	local({
		normal <- 1

		insist $ must_be_unlocked(
			'normal', environment(), invoking_call)
	})

message("insist $ must_be_whole")

	forall(
		"must be whole works for wholes",
		test_cases$num_positive_integer,
		insist $ must_be_whole(num, invoking_call)
	)

message("insist $ must_not_be_missing")

	forall(
		"must not be missing works",
		test_cases$collection,
		{
			f <- function (x) {
				insist $ must_not_be_missing(x)
			}

			f(coll)
		}
	)

message("insist $ must_exist")

	local({
		a <- 1
		insist $ must_exist('a', environment(), invoking_call)
	})

message("insist $ must_be_collection_of_equal_names")

c(
"must_be_collection_of_collections",
"must_be_collection_of_equal_length",
"must_be_collection_of_equal_names",
"must_be_collection_of_fn_matchable",

"must_be_collection_of_lengths",
"must_be_collections_of_length_grequal_than",

"must_be_collections_of_length_matching",
"must_be_correct_type",

"must_be_equal_length",
"must_be_existing_file",
"must_be_fn_matchable",

"must_be_fully_named",
"must_be_greater_than",
"must_be_grequal_than",

"must_be_logical_result",
"must_be_longer_or_equal_than",

"must_be_longer_than",
"must_be_matchable",
"must_be_nonnegative",

"must_be_non_primitive",
"must_be_of_length",
"must_be_parametres_of",

"must_be_recursive",
"must_be_unlistable",
"must_be_unlocked",

"must_not_be_missing_sym"
)

