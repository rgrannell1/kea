
kiwi ::: load_test_dependencies(environment())

as_typed_vector <- kiwi ::: as_typed_vector
as_atom         <- kiwi ::: as_atom

message("as_typed_vector (atomic)")

	over(coll) +

	describe("length-zero conversion always works") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		as_typed_vector(coll, 'numeric')    %is% numeric(0),
		as_typed_vector(coll, 'integer')    %is% integer(0),
		as_typed_vector(coll, 'double')     %is% double(0),
		as_typed_vector(coll, 'character')  %is% character(0),
		as_typed_vector(coll, 'logical')    %is% logical(0),
		as_typed_vector(coll, 'complex')    %is% complex(0),
		as_typed_vector(coll, 'raw')        %is% raw(0)
	) +

	describe("length-zero conversion always works (named)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		as_typed_vector(coll, 'numeric')    %is% as_named(numeric(0)),
		as_typed_vector(coll, 'integer')    %is% as_named(integer(0)),
		as_typed_vector(coll, 'double')     %is% as_named(double(0)),
		as_typed_vector(coll, 'character')  %is% as_named(character(0)),
		as_typed_vector(coll, 'logical')    %is% as_named(logical(0)),
		as_typed_vector(coll, 'complex')    %is% as_named(complex(0)),
		as_typed_vector(coll, 'raw')        %is% as_named(raw(0))
	) +

	run()

message("as_atom (atomic)")

	over(coll) +

	describe("") +
	holdsWhen() +

	run()

