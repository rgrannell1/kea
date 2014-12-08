
kea ::: load_test_dependencies(environment())

as_typed_vector <- kea ::: as_typed_vector
as_atom         <- kea ::: as_atom

# as_typed_vector and as_atomic are two of the most complex functions
# in Kea, so they are heavily tested.

message("as_typed_vector (atomic)")






	over(coll) +

	# --------------------- length-zero --------------------- #

	it("typed length-zero conversion always works for normal-modes") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 0 && !is_named(coll),

		as_typed_vector(coll, 'integer')    %is% integer(0),
		as_typed_vector(coll, 'double')     %is% double(0),
		as_typed_vector(coll, 'character')  %is% character(0),
		as_typed_vector(coll, 'logical')    %is% logical(0),
		as_typed_vector(coll, 'complex')    %is% complex(0),
		as_typed_vector(coll, 'raw')        %is% raw(0)
	) +

	it("typed length-zero conversion always works for normal-modes (named)") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 0 && is_named(coll),

		as_typed_vector(coll, 'integer')    %is% as_named(integer(0)),
		as_typed_vector(coll, 'double')     %is% as_named(double(0)),
		as_typed_vector(coll, 'character')  %is% as_named(character(0)),
		as_typed_vector(coll, 'logical')    %is% as_named(logical(0)),
		as_typed_vector(coll, 'complex')    %is% as_named(complex(0)),
		as_typed_vector(coll, 'raw')        %is% as_named(raw(0))
	) +

	it("typed length-zero conversion always works for numeric to integer") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 0 && !is_named(coll) && typeof(coll) == 'integer',

		as_typed_vector(coll, 'numeric') %is% integer(0)
	) +

	it("typed length-zero conversion always works for numeric to double") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 0 && !is_named(coll) && typeof(coll) == 'double',

		as_typed_vector(coll, 'numeric') %is% double(0)
	) +

	it("typed length-zero conversion always works for numeric to integer (named)") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 0 && is_named(coll) && typeof(coll) == 'integer',

		as_typed_vector(coll, 'numeric') %is% as_named(integer(0))
	) +

	it("typed length-zero conversion always works for numeric to double (named)") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 0 && is_named(coll) && typeof(coll) == 'double',

		as_typed_vector(coll, 'numeric') %is% as_named(double(0))
	) +

	# --------------------- length-one --------------------- #

	it("na conversion preserves names for non-raw") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 1
		&& all(is.na(coll) & !is.nan(coll)) && is_named(coll),

		names(as_typed_vector(coll, 'numeric'))   == names(coll),
		names(as_typed_vector(coll, 'integer'))   == names(coll),
		names(as_typed_vector(coll, 'double'))    == names(coll),
		names(as_typed_vector(coll, 'character')) == names(coll),
		names(as_typed_vector(coll, 'logical'))   == names(coll),
		names(as_typed_vector(coll, 'complex'))    == names(coll)
	) +

	it("na conversion changes type for non-raw") +
	holdsWhen(
		is_atomic(coll) && length(coll) == 1 &&
		all(is.na(coll) & !is.nan(coll)),

		typeof(as_typed_vector(coll, 'integer'))   == 'integer',
		typeof(as_typed_vector(coll, 'double'))    == 'double',
		typeof(as_typed_vector(coll, 'character')) == 'character',
		typeof(as_typed_vector(coll, 'logical'))   == 'logical',
		typeof(as_typed_vector(coll, 'complex'))    == 'complex'
	) +

	it('na conversion to numeric doesnt change integer') +
	holdsWhen(
		is_atomic(coll) && length(coll) == 1 && all(is.na(coll) & !is.nan(coll)) &&
		typeof(coll) == 'integer',

		typeof(as_typed_vector(coll, 'numeric')) == 'integer'
	) +

	it('na conversion to numeric doesnt change double') +
	holdsWhen(
		is_atomic(coll) && length(coll) == 1 && all(is.na(coll) & !is.nan(coll)) &&
		typeof(coll) == 'double',

		typeof(as_typed_vector(coll, 'numeric')) == 'double'
	) +

	it('na conversion to numeric changes other types') +
	holdsWhen(
		is_atomic(coll) && length(coll) == 1 && all(is.na(coll) & !is.nan(coll)) &&
		!any(typeof(coll) == c('integer', 'double')),

		typeof(as_typed_vector(coll, 'numeric')) == 'double'
	) +

	it("na conversion fails for raw") +
	failsWhen(
		is_atomic(coll) && length(coll) == 1 && 	all(is.na(coll) & !is.nan(coll)),

		as_typed_vector(coll, 'raw')
	) +

	# --------------------- length-any --------------------- #

	it("conversion to numeric works for integers") +
	holdsWhen(
		is_atomic(coll) && !is_named(coll) &&
		typeof(coll) == 'integer',

		as_typed_vector(coll, 'numeric') %is% coll
	) +

	it("conversion to numeric works for doubles") +
	holdsWhen(
		is_atomic(coll) && !is_named(coll) &&
		typeof(coll) == 'double',

		as_typed_vector(coll, 'numeric') %is% coll
	) +

	it("conversion to own type works for all types") +
	holdsWhen(
		is_atomic(coll),

		as_typed_vector(coll, typeof(coll)) %is% coll
	) +

	it("names are always conserved") +
	holdsWhen(
		is_atomic(coll),

		names( as_typed_vector(coll, typeof(coll)) ) %is% names(coll)
	) +

	run()






message("as_typed_vector (generic)")








message("as_atom (atomic)")






	over(coll) +

	it("atom length-zero conversion always works") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		as_atom(coll, 'numeric')   %is% numeric(0),
		as_atom(coll, 'integer')   %is% integer(0),
		as_atom(coll, 'double')    %is% double(0),
		as_atom(coll, 'character') %is% character(0),
		as_atom(coll, 'logical')   %is% logical(0),
		as_atom(coll, 'complex')   %is% complex(0),
		as_atom(coll, 'raw')       %is% raw(0)
	) +

	it("atom length-zero conversion always works (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		as_atom(coll, 'numeric')   %is% as_named(numeric(0)),
		as_atom(coll, 'integer')   %is% as_named(integer(0)),
		as_atom(coll, 'double')    %is% as_named(double(0)),
		as_atom(coll, 'character') %is% as_named(character(0)),
		as_atom(coll, 'logical')   %is% as_named(logical(0)),
		as_atom(coll, 'complex')   %is% as_named(complex(0)),
		as_atom(coll, 'raw')       %is% as_named(raw(0))
	) +

	run()






message("as_atom (generic")

