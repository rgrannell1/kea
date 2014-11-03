#!/usr/bin/env sh




echo "-- generating empty files for each kea function."

here=$(pwd)

for file in $(ls $here/R/ | egrep "x[A-Z]");
do

	touch "$here/inst/benchmarks/bench-$file"
	touch "$here/inst/examples/example-$file"
	touch "$here/tests/test-$file"

done





echo "-- ."
