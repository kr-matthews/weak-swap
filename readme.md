# Weak Swap
Weak swap is an optimization for a method of solving Rubik's cubes blindfolded, created by [Kevin Matthews](https://www.worldcubeassociation.org/persons/2010MATT02) and [Graham Siggins](https://www.worldcubeassociation.org/persons/2016SIGG01).
It is applicable when doing ECEC (memorize edges then corners, solve edges then corners).
For a given scramble, it will either not change the number of pairs to memorize and solve, or will decrease it by one.

The code here calculates how often it will save (decrease by) one pair.
It also does the calculations for a generalization, where you allow yourself to weak swap with any of k stickers (on k different pieces).

It would be ideal if somebody else independently did these calculations, as it's always possible that there is a mistake.
