throwdown
=========

An [R package](http://www.r-project.org/) to play a two player game of pairing.

Play a game by providing a sequence of numbers (typically 5).
The game is played in the following sequence:

1. player a reveals her first number
2. player b reveals his first two numbers
3. player a selects one of player b's revealed numbers to match with her revealed number
4. player a reveals her next two numbers
5. player b selects one of player a's revealed numbers to match with his revealed number
6. player b reveals his next two numbers
7. player a selects one of player b's revealed numbers to match with her revealed number
8. player a reveals her next two numbers
9. player b selects one of player a's revealed numbers to match with his revealed number
10. the remaining two numbers are matched.

A match is won by the player having the greatest number for each pair of numbers.
The winner of the game has won the most matches.

how to use this code
--------------------

This R package can be installed directly from GitHub using **devtools**.

```R
# install devtools for devtools::install_github
install.packages("devtools")
library(devtools)
# install throwdown
install_github("CSJCampbell/throwdown")
```

This package also includes tests in **testthat** format. 
From R run the call `test_package("throwdown")`.

A game can be played by providing for each player a sequence of numbers 
and a function which will pick a number in one of two ways, _pair_ or _reveal_.


```R
library(throwdown)
# a function which selects numbers 
pickerMax(active = 2, revealed = c(3, 1), type = "pair")
# play a game between two players
pairingGame(a = 1:5, b = c(5, 1:4), 
    strata = pickerRandom, stratb = pickerMax)
```
