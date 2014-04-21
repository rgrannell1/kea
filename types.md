# Type Signature Notation

- the container type of a variable must always be mentioned,
including for atomic values.

## Collections

* [any] or []: a list
* IanyI or II: a pairlist
* |any| or ||: a collection
* <any> or <>: a vector

## Functions

Variadic functions are denoted with a ...,
and their type is given

(a -> b -> ...c -> d)

nullary functions are denoted as

(-> a)

function that returns null

(a -> II)

Functions themselves have types; normal functions are denoted

(a -> b)

primitive functions are denoted 

|a -> b|

