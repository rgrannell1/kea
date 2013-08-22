
### Todo:

- [ ] #3 testthat issue: expect_equal(xSplit('.', 'ab'), c('a', 'b'))
- [ ] #9 several benchmarks will throw errors; for example xArity has type function -> int, but it will be given a collection. This needs to be fixed eventually.
- [ ] #10 move all tests over to property based testing. Contingent on me writing a decent generator combinator library first.
- [ ] #11 rename all predicates to pred, functions to fn, collections to ?
- [ ] #12 make xDropWhile more memory efficient by writing in terms of tail.
- [ ] #14 Important; make xSplitWith more efficient; currently poorly implemented.
- [ ] #15 Possible bug in xReducer: xReducer c 10:1 != 1:10. Investigate, fix, then close.

### Done:

- [x] #1 xLines and related function '' == character(0)? This may be wrong.
- [x] #2 bug in xSubstring('', 1); returns "NA", not error.
- [x] #6 make number functions generic, and operate on lists/pairlists of numbers too.
- [x] #7 ensure all function that take a collection/vector take a list or pairlist of length-one elements.
- [x] #4 xRest's type signature should include non-list collections.
- [x] #5 possible issue in functions using vapply; names are sometimes preserved. Check every function, then close issue.
- [x] #8 replace the parameter name 'f' with something more descriptive (closure?).
- [x] #13 add a description to xSplitWith.
- [x] #16 rename parameter "collection" to "coll".
