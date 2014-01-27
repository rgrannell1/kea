
# 1.
#
# find the smallest number evenly divisible by 1, 2, ..., 10
# this works a lot like a while loop that returns a value.

xIterate(
    num := {

        evenly_divisible <- num %% (1:10) == 0

        if (all(evenly_divisible)) {
            Return(num)
        } else {
            num + 1
        }
    },
    1
)

# 2. CE - Sum the first 1,000 primes
#    This is trivial to do with a fold, but interesting
#    to do with iterate and take,

# according to the prime number theorem,
# the number of primes is roughly
#
# y <- x / ln(x)
#
# we know y is 1000, but we want to know how many
# integers in 1...n we need to examine to find a thousand
# primes. An analytic function is no fun, so lets
# use a genetic algorithm to find the correct area to search.
#
# We have to make sure to overestimate the search space,
# since the prime density function has a certain error range.
# Otherwise we might find less than 1000 primes.
#
# In this case we use the sqrt(n) approximation to add
# an approximate error to the estimated search space.
#

is_prime <- num := {
    min(num %% 2:(num - 1)) > 0
}

prime_density <- x := {
    x / log(x)
}

upper_bound <-
x_(100000L) $
xIterate(
    estimate := {

        mutants <- estimate + c(
            +(estimate / 1000),
            -(estimate / 1000))

        remaining_error <- c(
            abs(prime_density(mutants[1]) - 1000),
            abs(prime_density(mutants[2]) - 1000))

        new_estimate <- mutants[which.min(remaining_error)]

        if (min(remaining_error) < 0.01) {
            Return(new_estimate)
        } else {
            new_estimate
        }
    }
) $
x_Tap(
    upper := {
        floor(sqrt(upper) + upper)
    }
)

# the sum of the first thousand primes.

prime_sum <-
x_(1:upper_bound) $
xSelect(is_prime) $
xTake(1000) $
xReduce('+')

# 3. CE Compute modulo without the builtin operator
#    This is trivial to do with xIterate.

modulo <- (num : modulus) := {
    # compute modulo without %%.

    x_(num) $
    x_Iterate(remainder := {

        if (remainder < modulus) {
            Return(remainder)
        } else {
            remainder - modulus
        }
    })
}

modulo(1121, 10)
# 1




# 1. CE Capitalise the first letter of each word in a sentence.
#

text <-
"Assyria was a major Semitic kingdom or empire of the Ancient Near East,
existing in various forms during a period of approximately nineteen
centuries from circa 2500 BC to 605 BC, spanning the Early Bronze Age
through to the late Iron Age. Centered on the Upper Tigris river, in
northern Mesopotamia(Iraq), the Assyrians came to rule powerful empires at
several times. As substantial part of the greater Mesopotamian 'cradle of
civilization', Assyria was at the height of technological, scientific and
cultural achievements for its time. At its peak, the Assyrian empire
stretched from the Mediterranean Sea to the Caspian Sea, and from the
foothills of the Caucasus to Arabia."

x_(text) $
xToWords() $
xMap(word := {

    chars <- xToChars(word)
    if (chars[1] %in% letters) {
        c(toupper(chars[1]), xRestOf(chars))
    } else {
        chars
    }
}) $
xMap(xFromChars) $
x_FromWords()

# "Assyria Was A Major Semitic Kingdom Or Empire Of The Ancient Near East,
# Existing In Various Forms During A Period Of Approximately Nineteen
# Centuries From Circa 2500 BC To 605 BC, Spanning The Early Bronze Age
# Through To The Late Iron Age. Centered On The Upper Tigris River, In
# Northern Mesopotamia(Iraq), The Assyrians Came To Rule Powerful Empires At
# Several Times. As Substantial Part Of The Greater Mesopotamian 'cradle Of
# Civilization', Assyria Was At The Height Of Technological, Scientific
# And Cultural Achievements For Its Time. At Its Peak, The Assyrian Empire
# Stretched From The Mediterranean Sea To The Caspian Sea, And From The
# Foothills Of The Caucasus To Arabia."
