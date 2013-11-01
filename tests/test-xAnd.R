
message("xAndLift")

assert( xAndLift(Truth, Truth)() %equals% True )
assert( xAndLift(Truth, Falsity)() %equals% False )
assert( xAndLift(Truth, Moot)() %equals% Na )
assert( xAndLift(Falsity, Truth)() %equals% False )
assert( xAndLift(Falsity, Falsity)() %equals% False )
assert( xAndLift(Falsity, Moot)() %equals% Na )
assert( xAndLift(Moot, Truth)() %equals% Na )
assert( xAndLift(Moot, Falsity)() %equals% Na )
assert( xAndLift(Moot, Moot)() %equals% Na )
