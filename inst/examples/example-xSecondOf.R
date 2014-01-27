
# 1. CE Return the penultimate string in a sentence.
#    Not the most efficient solution, but it's usually
#    sufficient. As always, using a right fold would be more
#    efficient but less expressive. Every problem can be solved
#    with a fold.

sentence <- "He just kept talking in one long incredibly unbroken sentence moving from
topic to topic so that no-one had a chance to
interrupt; it was really quite hypnotic"

x_(sentence) $
xToWords() $
xReverse() $
x_SecondOf()

# 'quite'
