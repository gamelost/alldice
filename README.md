AllDices is a restful service that allow you to query it with dice rolling expressions and it will return the result + additional information.

# Syntax

Syntax | Explaination | Example
-- | --
mdn | Roll m dices of n sides (1 .. n) | 1d6, 2d10
md[n1, ..., n2] | Roll m dices picking from the list of values | 2d[2,3,4]
+, -, *, /, % | Arithmetic | 2d6 + 4

# Limitations

Limitations as currently implemented

1. Max of 100 dice rolls at a time
2. Max of 100 sides or entries in a collection

# Future features

1. dice-pool types; sum, exploding (mxn), "open roll", "Collective tests"
2. additional arthmetic such as; min, max, least x, largest x
3. stats tracking on roll dices
4. session so each can have its own random generator or using global pool
5. Additive dice pool, target number dice pool, highest-die dice pools

# Reference

http://www.diku.dk/hjemmesider/ansatte/torbenm/Troll/quickRef.html
http://www.diku.dk/hjemmesider/ansatte/torbenm/Troll/manual.pdf
http://anydice.com/
http://divnull.com/rollplay/
http://semistable.com/dicelab/dicelab_manual.pdf
https://github.com/neilslater/games_dice

http://rpg.stackexchange.com/questions/20107/common-dice-mechanics
http://rpg.stackexchange.com/questions/15971/is-it-possible-to-produce-a-bowl-shaped-probability-curve-with-dice-rolls
http://rpg.stackexchange.com/questions/7131/systems-with-a-dice-mechanic-that-handles-very-easy-and-very-difficult-tasks
http://www.darkshire.net/jhkim/rpg/systemdesign/
