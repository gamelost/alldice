Alldice is a RESTful service along with a NSQ service that allows you to query it with dice rolling expressions (currently only scheme) and it will compute the roll and then return the result.

# Syntax

```
| ---------- | --------------------------- | ----------------- |
| Syntax     | Explaination                | Example           |
| ---------- | --------------------------- | ----------------- |
| (dice n)   | Roll a n side dice (1 .. n) | (dice 6)          |
| (roll n d) | Roll n dices                | (roll 2 (dice 6)) |
| (+ d y)    | Add y to the dice roll      | (+ (dice 6) 2)    |
| ---------- | --------------------------- | ----------------- |
```

# Limitations

There are currently no firm limitation implemented as it is. This software is probably quite
unsecure so use it at your own risk!  Eventually I would like to implement controllable limits such as:

1. Max of N dice rolls in one expression
2. Max amount of memory consumed in one expression
3. Max amount of cpu consumed in one expression

# Configuration

There is currently no real configuration available so the rough setup is:

1. Logging stream to StdErr
2. Ekg instance running at localhost:8081
3. RESTful Dice API at 0.0.0.0:8080
4. NSQ queue reader at: lethalcode.com on the alldice queue.

## RESTful Dice API

```
http://localhost:8080?src="(dice 6)"
```

## NSQ API

Request:
```
{
	"expression": "(dice 6)",
	"replyQueue": "foobar"
}
```

Reply:
```
{
	"expression": "(dice 6)",
	"result": "4",
	"error": ""
}
```

# Future features

1. Extended scheme buildins to support more fancy dice types such as: dice-pool types; sum, exploding (mxn), "open roll", "Collective tests", Additive dice pool, target number dice pool, highest-die dice pools
2. Additional arthmetic such as; min, max, least x, largest x
3. Probabilistic programming/rolls in which it computes the probability of the particular dice expression.
4. Implement other dice system such as PCGen, Roll20, and so on.

# Reference

1. http://www.diku.dk/hjemmesider/ansatte/torbenm/Troll/quickRef.html
2. http://www.diku.dk/hjemmesider/ansatte/torbenm/Troll/manual.pdf
3. http://anydice.com/
4. http://divnull.com/rollplay/
5. http://semistable.com/dicelab/dicelab_manual.pdf
6. https://github.com/neilslater/games_dice
7. http://rpg.stackexchange.com/questions/20107/common-dice-mechanics
8. http://rpg.stackexchange.com/questions/15971/is-it-possible-to-produce-a-bowl-shaped-probability-curve-with-dice-rolls
9. http://rpg.stackexchange.com/questions/7131/systems-with-a-dice-mechanic-that-handles-very-easy-and-very-difficult-tasks
10. http://www.darkshire.net/jhkim/rpg/systemdesign/
