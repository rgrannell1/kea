# Type Signature Notation

- the container type of a variable must always be mentioned,
including for atomic values.

## Collections

* **[any] or []**: a list
* **IanyI or II**: a pairlist
* **|any| or ||**: a collection
* **\<any\> or <>**: a vector

## Functions

Variadic functions are denoted with a ...,
and their type is given

**(a -> b -> ...c -> d)**

nullary functions are denoted as

**(-> a)**

function that returns null

**(a -> II)**

Functions themselves have types; normal functions are denoted

**(a -> b)**

primitive functions are denoted

**|a -> b|**

If the type signature of the function is undermined, use

**function**

## Vector Types

These are both types of vector, and subsets of these types with certain properties.

* **logical**: Logical values (True, False, Na)
* **boolean**: Logical true or false
* **integer**: A value of type integer
* **double** : A value of type double (don't use numeric)
* **number** : An integer or double
* **whole**  : A number divisible evenly by one, either integer or double.
