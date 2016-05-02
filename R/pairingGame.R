

#' @title Play Pairing Game
#' 
#' @description Play a game by providing a sequence of numbers (typically 5).
#' The game is played in the following sequence:\enumerate{
#'     \item player a reveals her first number
#'     \item player b reveals his first two numbers
#'     \item player a selects one of player b's revealed numbers to match with her revealed number
#'     \item player a reveals her next two numbers
#'     \item player b selects one of player a's revealed numbers to match with his revealed number
#'     \item player b reveals his next two numbers
#'     \item player a selects one of player b's revealed numbers to match with her revealed number
#'     \item player a reveals her next two numbers
#'     \item player b selects one of player a's revealed numbers to match with his revealed number
#'     \item the remaining two numbers are matched.
#' }
#' 
#' For each match, the player with the higher number wins one point.
#' The player with the most points wins the game.
#' The picker functions must have arguments 
#' active, revealed, type and exclude.
#' active and revealed must be numeric vectors. 
#' type must be a length 1 character vector with value "reveal" or "pair".
#' If type is "reveal", the function must pick the index of active to reveal.
#' If type is "pair", the function must pick index of revealed to pair.
#' exclude must be NA, or a numeric vector, 
#' which is the index of revealed to ignore when type is "reveal".
#' 
#' @param a numeric vector of player a pieces
#' @param b numeric vector of player b pieces of length a
#' @param strata picker function to select between two numbers for player a
#' @param stratb picker function to select between two numbers for player b
#' @return length 2 vector number of wins for player a and player b
#' @examples
#' pairingGame(a = 1:5, b = 5:1)
#' pairingGame(a = 10:14, b = 10:14)
#' pairingGame(a = 1:5, b = c(5, 1:4))
#' pairingGame(a = rep(1, 5), b = rep(1, 5), 
#'     strata = pickerFirst, stratb = pickerFirst)
#' pairingGame(a = 1:5, b = 5:1, strata = pickerRandom)
#' pairingGame(a = 1:5, b = c(5, 1:4), 
#'     strata = pickerRandom, stratb = pickerRandom)
#' pairingGame(a = 1:5, b = c(5, 1:4), 
#'     strata = pickerRandom, stratb = pickerMax)
#' pairingGame(a = 1:5, b = c(5, 1:4), 
#'     strata = pickerRandom, stratb = pickerJustMax)
#' set.seed(22532)
#' pairingGame(a = sample(1:5, size = 5), b = sample(1:5, size = 5), 
#'     strata = pickerRandom, stratb = pickerRandom)

pairingGame <- function(a, b, 
    strata = pickerFirst, stratb = pickerFirst) {
    
    nn <- length(a)
    if (length(b) != nn) { stop("length a and b must be the same") }
    indAIni <- strata(active = a, revealed = b, type = "reveal")
    
    aa <- bb <- numeric(nn)
    
    active <- a[indAIni]
    
    for (i in seq_len(nn)) {
        if (as.logical(i %% 2)) {
            # a revealed, b must choose one of active to reveal
            indBi1 <- stratb(active = b, revealed = active, 
                type = "reveal")
            indBi2 <- stratb(active = b, revealed = active, 
                exclude = indBi1, type = "reveal")
            if (!is.na(indBi2) && indBi2 == indBi1) { 
                indBi2 <- indBi2 + 1 
            }
            revealed <- na.omit(b[na.omit(c(indBi1, indBi2))])
            switch(as.character(length(revealed)),
                "2" = {
                    index <- strata(active = active, revealed = revealed)
                    aa[i] <- active
                    bb[i] <- revealed[index]
                    
                    a <- a[-which(a == active)[1]]
                    b <- b[-which(b == revealed[index])[1]]
                    active <- revealed[-index]
                }, 
                "1" = {
                    aa[i] <- active
                    bb[i] <- revealed
                    break
                },
                stop("player b must reveal one or two numbers")
            )
            
        } else {
            # b revealed, a must choose one of active to reveal
            indAi1 <- strata(active = a, revealed = active, 
                type = "reveal")
            indAi2 <- strata(active = a, revealed = active, 
                exclude = indAi1, type = "reveal")
            if (!is.na(indAi2) && indAi2 == indAi1) { 
                indAi2 <- indAi2 + 1 
            }
            revealed <- na.omit(a[na.omit(c(indAi1, indAi2))])
            switch(as.character(length(revealed)),
                "2" = {
                    index <- stratb(active = active, revealed = revealed)
                    bb[i] <- active
                    aa[i] <- revealed[index]
                    
                    b <- b[-which(b == active)[1]]
                    a <- a[-which(a == revealed[index])[1]]
                    active <- revealed[-index]
                },
                "1" = {
                    aa[i] <- revealed
                    bb[i] <- active
                    break
                },
                stop("player a must reveal one or two numbers")
            )
        }
        
    }
    c(a = sum(aa > bb), 
        b = sum(bb > aa))
}

