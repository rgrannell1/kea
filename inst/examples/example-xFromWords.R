
# 1. CodeEvalCapitalise the first letter of each word in a sentence.
#    The text is from Wikipedia.

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

    if (xIsIn(xFirstOf(chars), letters)) {
        c(toupper(xFirstOf(1)), xRestOf(chars))
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

