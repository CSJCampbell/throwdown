

#' @title Pick First Match Presented
#' 
#' @description Select index of first active or revealed.
#' If type is "reveal", pick index of active to reveal (always first).
#' If type is "pair", pick index of revealed to pair (always first).
#' 
#' @param active numeric vector choices of active player
#' @param revealed numeric vector choices revealed by non-active player for next round
#' @param exclude integer vector of indices of active 
#' to exclude when picking active for type reveal.
#' @param type single character, select modality:\itemize{
#'     \item "pair", pick index of revealed to create pairing.
#'     \item "reveal", pick index of active to reveal.
#' }
#' @return single numeric
#' @examples
#' pickerFirst(revealed = 1:2)
#' pickerFirst(revealed = 1)
#' pickerFirst(active = rep(1, 5), 
#'     revealed = rep(1, 5), type = "reveal")
#' pickerFirst(active = 1:5, 
#'     revealed = 1, exclude = 1, type = "reveal")

pickerFirst <- function(active, revealed, 
    exclude = NA,
    type = c("pair", "reveal")) {
    type <- match.arg(type)
    if ((missing(revealed) || all(is.na(revealed))) && type == "pair") {
        stop("revealed missing in pickerFirst")
    }
    if ((missing(active) || all(is.na(active))) && type == "reveal") { 
        stop("active is missing in pickerFirst") 
    }
    x <- switch(type, 
        "pair" = revealed,
        "reveal" = {
            active[na.omit(exclude)] <- NA
            active
        })
    seq_along(x)[!is.na(x)][1]
}

#' @title Pick a Match at Random
#' 
#' @description Selects index of random active or revealed.
#' If type is "reveal", pick index of 
#' active to reveal (always random).
#' If type is "pair", pick index of revealed to pair (always random).
#' @inheritParams pickerFirst
#' @param prob single numeric between 0 and 1
#' @return single numeric
#' @examples
#' pickerRandom(revealed = 3:4)
#' pickerRandom(revealed = 5)
#' pickerRandom(active = 1:5, revealed = 1:5, type = "reveal")
#' pickerRandom(active = 2, revealed = 2, exclude = 1)

pickerRandom <- function(active, revealed,
    exclude = NA,
    type = c("pair", "reveal")) {
    type <- match.arg(type)
    x <- switch(type,
        "pair" = seq_along(revealed),
        "reveal" = {
            x <- seq_along(active)
            x[na.omit(exclude)] <- NA
            x
        })
    if (length(na.omit(x)) < 1) { 
        NA_integer_
    } else {
        sample(x = seq_along(x)[!is.na(x)], size = 1L)
    }
}


#' @title Pick Match to Maximize Advantage over Revealed
#' 
#' @description Select index of active or revealed 
#' to give biggest victory by active.
#' If type is "reveal", pick index of active to reveal 
#' (always max unless win is not possible).
#' If type is "pair", pick index of revealed to pair 
#' (always max unless win is not possible).
#' If multiple matchups are equally advantageous, pick first.
#' 
#' @inheritParams pickerFirst
#' @return single numeric
#' @examples
#' pickerMax(active = 2, revealed = c(3, 1))
#' pickerMax(active = 1, revealed = 1:2)
#' pickerMax(active = 1, revealed = 1)
#' pickerMax(active = 2, revealed = c(3, 1), type = "reveal")
#' pickerMax(active = 1:5, revealed = c(3, 1), type = "reveal")
#' pickerMax(active = rep(5, 5), revealed = 1:5, type = "reveal")
#' pickerMax(active = rep(5, 5), revealed = 1:5, 
#'     exclude = 1, type = "reveal")
#' pickerMax(active = 4, revealed = 4, 
#'     exclude = 1)

pickerMax <- function(active, revealed, 
    exclude = NA,
    type = c("pair", "reveal")) {
    type <- match.arg(type)
    
    diffs <- switch(type, 
        "pair" = {
            active - revealed
        }, 
        "reveal" = {
            if (any(!is.na(exclude))) {
                active[na.omit(exclude)] <- NA_integer_
            }
            meds <- vapply(X = active, 
                FUN = function(a, b) { median(a - b, na.rm = TRUE) }, 
                FUN.VALUE = 0, 
                b = revealed)
            
            meds
        })
    ind <- which.max(diffs)
    if (length(ind) != 1) {
        ind <- NA_integer_
    }
    ind
}

#' @title Pick Match to Best Revealed by Smallest Margin
#' @description Select index of active or revealed 
#' to give biggest victory by active.
#' If type is "reveal", pick index of active to reveal 
#' (always just max unless win is not possible).
#' If type is "pair", pick index of revealed to pair 
#' (always just max unless win is not possible).
#' @param active single numeric
#' @param revealed length 1 or 2 numeric
#' @return length 1 numeric value 1
#' @examples
#' pickerJustMax(active = 2, revealed = c(3, 1))
#' pickerJustMax(active = 3, revealed = 1:2)
#' pickerJustMax(active = 3, revealed = 4:5)
#' pickerJustMax(active = 1, revealed = 1)
#' pickerJustMax(active = 1, revealed = 5)
#' pickerJustMax(active = 1:5, revealed = 1:5, type = "reveal")
#' pickerJustMax(active = 1:5, revealed = rep(3, times = 5), type = "reveal")

pickerJustMax <- function(active, revealed, 
    exclude = NA,
    type = c("pair", "reveal")) {
    type <- match.arg(type)
    countWins <- function(r, y) { 
        wins <- r > y
        # draws
        wins[r == y] <- 0.5
        sum(wins, na.rm = TRUE)
    }
    ind <- switch(type, 
        # pick least unless exactly one winner
        "pair" = {
            if (length(na.omit(revealed)) < 1) {
                NA_integer_
            } else {
                # handle multi-length active
                opts <- vapply(X = revealed, 
                    FUN = countWins, 
                    FUN.VALUE = 0, 
                    y = active)
                switch(as.character(sum(opts == max(opts, na.rm = TRUE), na.rm = TRUE)),
                    "0" = which.min(revealed),
                    "1" = which(opts == max(opts, na.rm = TRUE)), 
                    {
                        if (all(opts == max(opts, na.rm = TRUE))) {
                            which.min(revealed)
                        } else {
                            diffs <- vapply(X = revealed, 
                                FUN = function(r, a) { which.max(a - r) }, 
                                FUN.VALUE = 0, 
                                a = active)
                            cat(paste(diffs, collapse = ", "), "\n")
                            diffs[diffs <= 0] <- Inf
                            which.min(diffs)
                        }
                    }
                )
            }
        },
        "reveal" = {
            if (any(!is.na(exclude))) {
                active[exclude] <- NA_integer_
            }
            if (length(na.omit(active)) < 1) {
                NA_integer_
            } else {
                opts <- vapply(X = active, 
                    FUN = countWins, 
                    FUN.VALUE = 0, 
                    y = revealed)
                if (all(opts <= 0)) {
                    ind <- which.min(active)
                } else {
                    val <- min(active[opts == max(opts, na.rm = TRUE)], na.rm = TRUE)
                    ind <- active == val
                    ind <- switch(as.character(sum(ind, na.rm = TRUE)),
                        "0" = which.min(active),
                        "1" = which(ind),
                        {
                            pickerJustMax(active = active, 
                                revealed = revealed, 
                                type = "pair")
                        }
                    )
                }
            }
        }
    )
    if (length(ind) == 0) { ind <- NA_integer_ }
    ind
}

