	
### Todo:

- [ ] #3 testthat issue: expect_equal(xSplit('.', 'ab'), c('a', 'b'))
- [ ] #8 replace the parameter name 'f' with something more descriptive (closure?).
- [ ] #9 several benchmarks will throw errors; for example xArity has type function -> int, but it will be given a collection. This needs to be fixed eventually.
- [ ] #10 move all tests over to property based testing. Contingent on me writing a decent generator combinator library first.
- [ ] #11 rename all predicates to pred, functions to fn, collections to ?

### Done:

- [x] #1 xLines and related function '' == character(0)? This may be wrong.
- [x] #2 bug in xSubstr('', 1); returns "NA", not error.
- [x] #6 make number functions generic, and operate on lists/pairlists of numbers too.
- [x] #7 ensure all function that take a collection/vector take a list or pairlist of length-one elements.
- [x] #4 xRest's type signature should include non-list collections.
- [x] #5 possible issue in functions using vapply; names are sometimes preserved. Check every function, then close issue.
