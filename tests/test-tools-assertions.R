
insist <- arrow:::insist
forall <- arrow:::forall
test_cases <- arrow:::test_cases
format_call <- arrow:::format_call

invoking_call <- format_call(
	quote(xMap(x := x^2, coll = 1:10)) )

message("must_be_logical_result")

	test_sym <- NULL

	forall(
		"must_be_logical_result works for logicals.",
		test_cases$logical,
		insist $ must_be_logical_result(
			coll, test_sym, invoking_call)
	)

message("max_must_be_less_than_length_of")

message("minimum_must_be_greater_than")

message("must_be_character")

	forall(
		"must be must_be_character works",
		test_cases$str_word,
		insist $ must_be_character(str, invoking_call)
	)

message("must_be_collection")

	forall(
		"must be must_be_character works",
		test_cases$collection,
		insist $ must_be_collection(coll, invoking_call)
	)

message("must_be_collection_of_collections")

message("must_be_collection_of_equal_length")

message("must_be_collection_of_fn_matchable")

message("must_be_collection_of_lengths")

message("must_be_collections_of_length_matching")

message("must_be_equal_length")

message("must_be_fn_matchable")

message("must_be_fully_named")

message("must_be_greater_than")

message("must_be_grequal_than")

message("must_be_longer_or_equal_than")

message("must_be_longer_than")

message("must_be_non_primitive")

message("must_be_nonnegative")

message("must_be_of_length")

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

message("must_be_parametres_of")

	local({

		fn <- function (a, b, c) {
			a + b + c
		}

		insist $ must_be_parametres_of(
			c('a', 'b'), fn, invoking_call)

	})

message("must_be_recursive")

	forall(
		"must be recursive works for lists & pairlists",
		test_cases$collection,
		insist $ must_be_recursive(as.list(coll), invoking_call)
	)

message("must_be_unlocked")

	local({
		normal <- 1

		insist $ must_be_unlocked(
			'normal', environment(), invoking_call)
	})

message("must_be_whole")

	forall(
		"must be whole works for wholes",
		test_cases$num_positive_integer,
		insist $ must_be_whole(num, invoking_call)
	)

message("must_exist")

	local({
		a <- 1
		insist $ must_exist('a', environment(), invoking_call)
	})
