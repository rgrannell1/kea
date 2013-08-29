
### Todo:

- [ ] #3 testthat issue: expect_equal(xSplit('.', 'ab'), c('a', 'b'))

- [ ] #9 several benchmarks will throw errors; for example xArity has type function -> int, but it will be given a collection. This needs to be fixed eventually.

- [ ] #10 move all tests over to property based testing. Contingent on me writing a decent generator combinator library first.

- [ ] #12 make xDropWhile more memory efficient by writing in terms of tail.

- [ ] #14 Important; make xSplitWith more efficient; currently poorly implemented.

- [ ] #15 Possible bug in xReducer: xReducer c 10:1 != 1:10. Investigate, fix, then close.

- [ ] #21 ensure that the right-hand of a x_()$method call is always a valid input for that method; currently
    incorrect types are allowed.

- [ ] #25 check that functions that should return [A](0) do return such a value.

- [] #26 get does not work with xFixDefaults. Figure out why.

- [ ] #27 implement pattern matching (see core.match documentation), spend up to 500 lines for version 0.1.

- [ ] #28 implement autopartial, in a completely reliable way.

- [ ] #29 make chars and other string methods generic to collecton type.

- [ ] #30 should has defaults return names? list?

- [ ] #31 ensure all vector functions return vector[0] if input is empty, or no match found.

- [ ] #32 document splitwith.

- [ ] #33 implement flatten coll num.

- [ ] #34 rename xCount, xFixDefaults, xFmap, xHasDefaults, xPairsAsList, 

xSplitAt, xSplitStr, xSubStr, xTmap.
xSplitAt -> xSplit. 
xCount -> xPoll. 
xPosition -> xLocate. 
xCompress -> xPack.
xSplitString -> xSplitStr. 
xSubString -> xSubStr.
xTmap -> . 
xFmap -> . 
xParameters <- xParams
xFixDefaults -> .
xHasDefaults -> .
xPairsAsList -> . 
xConcatMap -> xFlatMap.

### Done:

- [x] #1 xLines and related function '' == character(0)? This may be wrong.
- [x] #2 bug in xSubStr('', 1); returns "NA", not error.
- [x] #6 make number functions generic, and operate on lists/pairlists of numbers too.
- [x] #7 ensure all function that take a collection/vector take a list or pairlist of length-one elements.
- [x] #4 xRest's type signature should include non-list collections.
- [x] #5 possible issue in functions using vapply; names are sometimes preserved. Check every function, then close issue.
- [x] #8 replace the parameter name 'f' with something more descriptive (closure?).
- [x] #13 add a description to xSplitWith.
- [x] #16 rename parameter "collection" to "coll".
- [x] #11 rename all predicates to pred, functions to fn, collections to ?
- [x] #20 reimplemented the x_( ) monad, without partial application and with acceptable efficiency.
- [x] #17 force (a -> boolean) functions to throw an error is non-boolean values is returned.
- [x] #18 move from requiring boolean to requiring logical functions, since it's more idiomatic.
- [x] #24 check xUntil and xWhile logical handling.
- [x] #22 change map -> xmap in pluck
- [x] #23 alias x...l methods with x ..., to prevent user typos.
- [x] #19 implement a pairs to list function [a, b] -> {a: b}
- [x] #33 implement a map vectorisation function.

### Won't Fix

- [x] #29 wrap all match.fun statements with xAsClosure(), for genericitiy.
Broken for certain primitives (c).

