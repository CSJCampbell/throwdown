

context("play pairingGame")

test_that("normal case", {
    
    draw <- structure(c(2L, 2L), .Names = c("a", "b"))
    
    expect_equal(object = pairingGame(a = 1:5, b = 5:1),
        expected = draw)
    
    expect_equal(object = pairingGame(a = 10:14, b = 10:14),
        expected = structure(c(0L, 0L), .Names = c("a", "b")))
    
    expect_equal(object = pairingGame(a = 1:5, b = c(5, 1:4)),
        expected = structure(c(4L, 1L), .Names = c("a", "b")))
    
    expect_equal(object = pairingGame(a = rep(1, 5), b = rep(1, 5), 
            strata = pickerFirst, stratb = pickerFirst),
        expected = structure(c(0L, 0L), .Names = c("a", "b")))
    
    set.seed(21532)
    expect_equal(object = pairingGame(a = 1:5, b = 5:1, 
            strata = pickerRandom),
        expected = structure(c(2L, 3L), .Names = c("a", "b")))
    
    set.seed(21531)
    expect_equal(object = pairingGame(a = 11:15, b = 15:11, 
        strata = pickerRandom),
        expected = draw)
    
    set.seed(22132)
    expect_equal(object = pairingGame(a = 1:5, b = c(5, 1:4), 
            strata = pickerRandom, stratb = pickerRandom),
        expected = draw)
    
    set.seed(22513)
    expect_equal(object = pairingGame(a = 1:5, b = c(5, 1:4), 
            strata = pickerRandom, stratb = pickerMax),
        expected = draw)
    
    set.seed(22531)
    expect_equal(object = pairingGame(a = 1:5, b = c(5, 1:4), 
            strata = pickerRandom, stratb = pickerJustMax),
        expected = draw)
    
    set.seed(12532)
    expect_equal(object = pairingGame(a = sample(1:5, size = 5), b = sample(1:5, size = 5), 
            strata = pickerRandom, stratb = pickerRandom),
        expected = structure(c(3L, 2L), .Names = c("a", "b")))
    
    expect_equal(object = pairingGame(a = c(4L, 1L, 2L, 5L, 3L), 
        b = c(3L, 4L, 1L, 2L, 5L), 
        strata = pickerJustMax, stratb = pickerJustMax),
        expected = draw)
})

test_that("manually check unexpected cases", {
    
    draw <- structure(c(2L, 2L), .Names = c("a", "b"))
    
    ###########################################################################
    # pickerMax vs pickerFirst
    ###########################################################################
    
    aa <- a <- c(4L, 1L, 2L, 5L, 3L)
    bb <- b <- c(3L, 4L, 1L, 2L, 5L)
    posa <- numeric(5)
    posb <- numeric(5)
    # player a reveals her first number
    pickerMax(active = a, revealed = b, type = "reveal")
    # 4
    # player b reveals his first two numbers
    pickerFirst(active = b, revealed = a[4], type = "reveal")
    # 1
    pickerFirst(active = b, revealed = a[4], exclude = 1, type = "reveal")
    # 2
    # player a selects one of player b's revealed numbers to match with her revealed number
    pickerMax(active = a[4], revealed = b[c(1, 2)])
    # 1
    posa[1] <- 4
    posb[1] <- 1
    a[4] <- NA
    b[1] <- NA
    # player a reveals her next two numbers
    pickerMax(active = a, revealed = b[2], type = "reveal")
    # 1
    pickerMax(active = a, revealed = b[2], exclude = 1, type = "reveal")
    # 5
    # player b selects one of player a's revealed numbers to match with his revealed number
    pickerFirst(active = b[2], revealed = a[c(1, 5)])
    # 1
    posa[2] <- 1
    posb[2] <- 2
    a[1] <- NA
    b[2] <- NA
    # player b reveals his next two numbers
    pickerFirst(active = b, revealed = a[5], type = "reveal")
    # 3
    pickerFirst(active = b, revealed = a[5], exclude = 3, type = "reveal")
    # 4
    # player a selects one of player b's revealed numbers to match with her revealed number
    pickerMax(active = a[5], revealed = b[c(3, 4)])
    # 1
    posa[3] <- 5
    posb[3] <- 3
    a[5] <- NA
    b[3] <- NA
    # player a reveals her next two numbers
    pickerMax(active = a, revealed = b[4], type = "reveal")
    # 3
    pickerMax(active = a, revealed = b[4], exclude = 3, type = "reveal")
    # 2
    # player b selects one of player a's revealed numbers to match with his revealed number
    pickerFirst(active = b[4], revealed = a[c(3, 2)])
    # 1
    posa[4] <- 3
    posb[4] <- 4
    a[3] <- NA
    b[4] <- NA
    # the remaining two numbers are matched.
    posa[5] <- 2
    posb[5] <- 5
    a[2] <- NA
    b[5] <- NA
    
    # player a wins 2 to 1
    res <- c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerMax, stratb = pickerFirst),
        expected = res)
    
    ###########################################################################
    # pickerJustMax vs pickerMax
    ###########################################################################
    
    aa <- a <- c(4L, 1L, 2L, 5L, 3L)
    bb <- b <- c(3L, 4L, 1L, 2L, 5L)
    posa <- numeric(5)
    posb <- numeric(5)
    # player a reveals her first number
    pickerJustMax(active = a, revealed = b, type = "reveal")
    # 4
    # player b reveals his first two numbers
    pickerMax(active = b, revealed = a[4], type = "reveal")
    # 5
    pickerMax(active = b, revealed = a[4], exclude = 5, type = "reveal")
    # 2
    # player a selects one of player b's revealed numbers to match with her revealed number
    pickerJustMax(active = a[4], revealed = b[c(5, 2)])
    # 1
    posa[1] <- 4
    posb[1] <- 5
    a[4] <- NA
    b[5] <- NA
    # player a reveals her next two numbers
    pickerJustMax(active = a, revealed = b[2], type = "reveal")
    # 1
    pickerJustMax(active = a, revealed = b[2], exclude = 1, type = "reveal")
    # 2
    # player b selects one of player a's revealed numbers to match with his revealed number
    pickerMax(active = b[2], revealed = a[c(1, 2)])
    # 2
    posa[2] <- 2
    posb[2] <- 2
    a[2] <- NA
    b[2] <- NA
    # player b reveals his next two numbers
    pickerMax(active = b, revealed = a[1], type = "reveal")
    # 1
    pickerMax(active = b, revealed = a[1], exclude = 1, type = "reveal")
    # 4
    # player a selects one of player b's revealed numbers to match with her revealed number
    pickerJustMax(active = a[1], revealed = b[c(1, 4)])
    # 2
    posa[3] <- 1
    posb[3] <- 4
    a[1] <- NA
    b[4] <- NA
    # player a reveals her next two numbers
    pickerJustMax(active = a, revealed = b[1], type = "reveal")
    # 5
    pickerJustMax(active = a, revealed = b[1], exclude = 5, type = "reveal")
    # 3
    # player b selects one of player a's revealed numbers to match with his revealed number
    pickerMax(active = b[1], revealed = a[c(5, 3)])
    # 2
    posa[4] <- 3
    posb[4] <- 1
    a[3] <- NA
    b[1] <- NA
    # the remaining two numbers are matched.
    posa[5] <- 5
    posb[5] <- 3
    a[5] <- NA
    b[3] <- NA
    
    # draw 2 to 2
    res <- c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    expect_equal(object = res, expected = draw)
    
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerJustMax, stratb = pickerMax),
        expected = draw)
    
    ###########################################################################
    # pickerMax vs pickerMax
    ###########################################################################
    
    aa <- a <- c(4L, 1L, 2L, 5L, 3L)
    bb <- b <- c(3L, 4L, 1L, 2L, 5L)
    posa <- numeric(5)
    posb <- numeric(5)
    # player a reveals her first number
    pickerMax(active = a, revealed = b, type = "reveal")
    # 4
    # player b reveals his first two numbers
    pickerMax(active = b, revealed = a[4], type = "reveal")
    # 5
    pickerMax(active = b, revealed = a[4], exclude = 5, type = "reveal")
    # 2
    # player a selects one of player b's revealed numbers to match with her revealed number
    pickerMax(active = a[4], revealed = b[c(5, 2)])
    # 2
    posa[1] <- 4
    posb[1] <- 2
    a[4] <- NA
    b[2] <- NA
    # player a reveals her next two numbers
    pickerMax(active = a, revealed = b[5], type = "reveal")
    # 1
    pickerMax(active = a, revealed = b[5], exclude = 1, type = "reveal")
    # 5
    # player b selects one of player a's revealed numbers to match with his revealed number
    pickerMax(active = b[5], revealed = a[c(1, 5)])
    # 2
    posa[2] <- 5
    posb[2] <- 5
    a[5] <- NA
    b[5] <- NA
    # player b reveals his next two numbers
    pickerMax(active = b, revealed = a[1], type = "reveal")
    # 1
    pickerMax(active = b, revealed = a[1], exclude = 1, type = "reveal")
    # 4
    # player a selects one of player b's revealed numbers to match with her revealed number
    pickerMax(active = a[1], revealed = b[c(1, 4)])
    # 2
    posa[3] <- 1
    posb[3] <- 4
    a[1] <- NA
    b[4] <- NA
    # player a reveals her next two numbers
    pickerMax(active = a, revealed = b[1], type = "reveal")
    # 3
    pickerMax(active = a, revealed = b[1], exclude = 3, type = "reveal")
    # 2
    # player b selects one of player a's revealed numbers to match with his revealed number
    pickerMax(active = b[1], revealed = a[c(3, 2)])
    # 2
    posa[4] <- 2
    posb[4] <- 1
    a[2] <- NA
    b[1] <- NA
    # the remaining two numbers are matched.
    posa[5] <- 3
    posb[5] <- 3
    a[3] <- NA
    b[3] <- NA
    
    # player a wins 3 to 2
    res <- c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerMax, stratb = pickerMax),
        expected = res)
    
    ###########################################################################
    # pickerFirst vs pickerJustMax
    ###########################################################################
    
    aa <- a <- c(4L, 1L, 2L, 5L, 3L)
    bb <- b <- c(3L, 4L, 1L, 2L, 5L)
    posa <- numeric(5)
    posb <- numeric(5)
    # player a reveals her first number
    pa01 <- pickerFirst(active = a, revealed = b, type = "reveal")
    pa01 == 1
    # player b reveals his first two numbers
    pb01 <- pickerJustMax(active = b, revealed = a[pa01], type = "reveal")
    pb01 == 5
    pb02 <- pickerJustMax(active = b, revealed = a[pa01], exclude = pb01, type = "reveal")
    pb02 == 2
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa02 <- pickerFirst(active = a[pa01], revealed = b[c(pb01, pb02)])
    pa02 == 1
    posa[1] <- pa01
    posb[1] <- c(pb01, pb02)[pa02]
    a[posa[1]] <- NA
    b[posb[1]] <- NA
    # player a reveals her next two numbers
    pa03 <- pickerFirst(active = a, revealed = b[c(pb01, pb02)[-pa02]], type = "reveal")
    pa03 == 2
    pa04 <- pickerFirst(active = a, revealed = b[c(pb01, pb02)[-pa02]], exclude = pa03, type = "reveal")
    pa04 == 3
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb03 <- pickerJustMax(active = b[c(pb01, pb02)[-pa02]], revealed = a[c(pa03, pa04)])
    pb03 == 1
    posa[2] <- c(pa03, pa04)[pb03]
    posb[2] <- c(pb01, pb02)[-pa02]
    a[posa[2]] <- NA
    b[posb[2]] <- NA
    # player b reveals his next two numbers
    pb04 <- pickerJustMax(active = b, revealed = a[c(pa03, pa04)[-pb03]], type = "reveal")
    pb04 == 1
    pb05 <- pickerJustMax(active = b, revealed = a[c(pa03, pa04)[-pb03]], exclude = pb04, type = "reveal")
    pb05 == 4
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa06 <- pickerFirst(active = a[c(pa03, pa04)[-pb03]], revealed = b[c(pb04, pb05)])
    pa06 == 1
    posa[3] <- c(pa03, pa04)[-pb03]
    posb[3] <- c(pb04, pb05)[pa06]
    a[posa[3]] <- NA
    b[posb[3]] <- NA
    # player a reveals her next two numbers
    pa07 <- pickerFirst(active = a, revealed = b[c(pb04, pb05)[-pa06]], type = "reveal")
    pa07 == 4
    pa08 <- pickerFirst(active = a, revealed = b[c(pb04, pb05)[-pa06]], exclude = pa07, type = "reveal")
    pa08 == 5
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb07 <- pickerJustMax(active = b[c(pb04, pb05)[-pa06]], revealed = a[c(pa07, pa08)])
    pb07 == 2
    posa[4] <- c(pa07, pa08)[pb07]
    posb[4] <- c(pb04, pb05)[-pa06]
    a[posa[4]] <- NA
    b[posb[4]] <- NA
    # the remaining two numbers are matched.
    posa[5] <- c(pa07, pa08)[-pb07]
    posb[5] <- (1:5)[-c(posb)]
    
    # a loses 2 to 3
    res <- c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerFirst, stratb = pickerJustMax),
        expected = res)

    
    ###########################################################################
    # pickerFirst vs pickerJustMax
    ###########################################################################
    
    aa <- a <- 1:5
    bb <- b <- c(3L, 4L, 1L, 2L, 5L)
    posa <- numeric(5)
    posb <- numeric(5)
    # player a reveals her first number
    pa01 <- pickerFirst(active = a, revealed = b, type = "reveal")
    pa01 == 1
    # player b reveals his first two numbers
    pb01 <- pickerJustMax(active = b, revealed = a[pa01], type = "reveal")
    pb01 == 4
    pb02 <- pickerJustMax(active = b, revealed = a[pa01], exclude = pb01, type = "reveal")
    pb02 == 1
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa02 <- pickerFirst(active = a[pa01], revealed = b[c(pb01, pb02)])
    pa02 == 1
    posa[1] <- pa01
    posb[1] <- c(pb01, pb02)[pa02]
    a[posa[1]] <- NA
    b[posb[1]] <- NA
    # player a reveals her next two numbers
    pa03 <- pickerFirst(active = a, revealed = b[c(pb01, pb02)[-pa02]], type = "reveal")
    pa03 == 2
    pa04 <- pickerFirst(active = a, revealed = b[c(pb01, pb02)[-pa02]], exclude = pa03, type = "reveal")
    pa04 == 3
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb03 <- pickerJustMax(active = b[c(pb01, pb02)[-pa02]], revealed = a[c(pa03, pa04)])
    pb03 == 2
    posa[2] <- c(pa03, pa04)[pb03]
    posb[2] <- c(pb01, pb02)[-pa02]
    a[posa[2]] <- NA
    b[posb[2]] <- NA
    # player b reveals his next two numbers
    pb04 <- pickerJustMax(active = b, revealed = a[c(pa03, pa04)[-pb03]], type = "reveal")
    pb04 == 2
    pb05 <- pickerJustMax(active = b, revealed = a[c(pa03, pa04)[-pb03]], exclude = pb04, type = "reveal")
    pb05 == 5
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa06 <- pickerFirst(active = a[c(pa03, pa04)[-pb03]], revealed = b[c(pb04, pb05)])
    pa06 == 1
    posa[3] <- c(pa03, pa04)[-pb03]
    posb[3] <- c(pb04, pb05)[pa06]
    a[posa[3]] <- NA
    b[posb[3]] <- NA
    # player a reveals her next two numbers
    pa07 <- pickerFirst(active = a, revealed = b[c(pb04, pb05)[-pa06]], type = "reveal")
    pa07 == 4
    pa08 <- pickerFirst(active = a, revealed = b[c(pb04, pb05)[-pa06]], exclude = pa07, type = "reveal")
    pa08 == 5
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb07 <- pickerJustMax(active = b[c(pb04, pb05)[-pa06]], revealed = a[c(pa07, pa08)])
    pb07 == 2
    posa[4] <- c(pa07, pa08)[pb07]
    posb[4] <- c(pb04, pb05)[-pa06]
    a[posa[4]] <- NA
    b[posb[4]] <- NA
    # the remaining two numbers are matched.
    posa[5] <- c(pa07, pa08)[-pb07]
    posb[5] <- (1:5)[-c(posb)]
    
    # a loses 1 to 2
    res <- c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerFirst, stratb = pickerJustMax),
        expected = res)
    
    ###########################################################################
    # pickerRandom vs pickerJustMax
    ###########################################################################
    
    aa <- a <- c(4L, 1L, 2L, 5L, 3L)
    bb <- b <- c(3L, 4L, 1L, 2L, 5L)
    posa <- numeric(5)
    posb <- numeric(5)
    set.seed(3456)
    # player a reveals her first number
    pa01 <- pickerRandom(active = a, revealed = b, type = "reveal")
    pa01 == 4
    # player b reveals his first two numbers
    pb01 <- pickerJustMax(active = b, revealed = a[pa01], type = "reveal")
    pb01 == 5
    pb02 <- pickerJustMax(active = b, revealed = a[pa01], exclude = pb01, type = "reveal")
    pb02 == 3
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa02 <- pickerRandom(active = a[pa01], revealed = b[c(pb01, pb02)])
    pa02 == 2
    posa[1] <- pa01
    posb[1] <- c(pb01, pb02)[pa02]
    a[posa[1]] <- NA
    b[posb[1]] <- NA
    # player a reveals her next two numbers
    pa03 <- pickerRandom(active = a, revealed = b[c(pb01, pb02)[-pa02]], type = "reveal")
    pa03 == 5
    pa04 <- pickerRandom(active = a, revealed = b[c(pb01, pb02)[-pa02]], exclude = pa03, type = "reveal")
    pa04 == 2
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb03 <- pickerJustMax(active = b[c(pb01, pb02)[-pa02]], revealed = a[c(pa03, pa04)])
    pb03 == 2
    posa[2] <- c(pa03, pa04)[pb03]
    posb[2] <- c(pb01, pb02)[-pa02]
    a[posa[2]] <- NA
    b[posb[2]] <- NA
    # player b reveals his next two numbers
    pb04 <- pickerJustMax(active = b, revealed = a[c(pa03, pa04)[-pb03]], type = "reveal")
    pb04 == 2
    pb05 <- pickerJustMax(active = b, revealed = a[c(pa03, pa04)[-pb03]], exclude = pb04, type = "reveal")
    pb05 == 1
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa06 <- pickerRandom(active = a[c(pa03, pa04)[-pb03]], revealed = b[c(pb04, pb05)])
    pa06 == 2
    posa[3] <- c(pa03, pa04)[-pb03]
    posb[3] <- c(pb04, pb05)[pa06]
    a[posa[3]] <- NA
    b[posb[3]] <- NA
    # player a reveals her next two numbers
    pa07 <- pickerRandom(active = a, revealed = b[c(pb04, pb05)[-pa06]], type = "reveal")
    pa07 == 3
    pa08 <- pickerRandom(active = a, revealed = b[c(pb04, pb05)[-pa06]], exclude = pa07, type = "reveal")
    pa08 == 1
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb07 <- pickerJustMax(active = b[c(pb04, pb05)[-pa06]], revealed = a[c(pa07, pa08)])
    pb07 == 2
    posa[4] <- c(pa07, pa08)[pb07]
    posb[4] <- c(pb04, pb05)[-pa06]
    a[posa[4]] <- NA
    b[posb[4]] <- NA
    # the remaining two numbers are matched.
    posa[5] <- c(pa07, pa08)[-pb07]
    posb[5] <- (1:5)[-c(posb)]
    
    # draw
    res <- c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    set.seed(3456)
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerRandom, stratb = pickerJustMax),
        expected = res)
    
    ###########################################################################
    # pickerJustMax vs pickerRandom
    ###########################################################################
    
    aa <- a <- c(4L, 1L, 2L, 5L, 3L)
    bb <- b <- c(3L, 4L, 1L, 2L, 5L)
    posa <- numeric(5)
    posb <- numeric(5)
    set.seed(13456)
    # player a reveals her first number
    pa01 <- pickerJustMax(active = a, revealed = b, type = "reveal")
    pa01 == 4
    # player b reveals his first two numbers
    pb01 <- pickerRandom(active = b, revealed = a[pa01], type = "reveal")
    pb01 == 5
    pb02 <- pickerRandom(active = b, revealed = a[pa01], exclude = pb01, type = "reveal")
    pb02 == 1
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa02 <- pickerJustMax(active = a[pa01], revealed = b[c(pb01, pb02)])
    pa02 == 1
    posa[1] <- pa01
    posb[1] <- c(pb01, pb02)[pa02]
    a[posa[1]] <- NA
    b[posb[1]] <- NA
    # player a reveals her next two numbers
    pa03 <- pickerJustMax(active = a, revealed = b[c(pb01, pb02)[-pa02]], type = "reveal")
    pa03 == 1
    pa04 <- pickerJustMax(active = a, revealed = b[c(pb01, pb02)[-pa02]], exclude = pa03, type = "reveal")
    pa04 == 5
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb03 <- pickerRandom(active = b[c(pb01, pb02)[-pa02]], revealed = a[c(pa03, pa04)])
    pb03 == 1
    posa[2] <- c(pa03, pa04)[pb03]
    posb[2] <- c(pb01, pb02)[-pa02]
    a[posa[2]] <- NA
    b[posb[2]] <- NA
    # player b reveals his next two numbers
    pb04 <- pickerRandom(active = b, revealed = a[c(pa03, pa04)[-pb03]], type = "reveal")
    pb04 == 3
    pb05 <- pickerRandom(active = b, revealed = a[c(pa03, pa04)[-pb03]], exclude = pb04, type = "reveal")
    pb05 == 4
    # player a selects one of player b's revealed numbers to match with her revealed number
    pa06 <- pickerJustMax(active = a[c(pa03, pa04)[-pb03]], revealed = b[c(pb04, pb05)])
    pa06 == 1
    posa[3] <- c(pa03, pa04)[-pb03]
    posb[3] <- c(pb04, pb05)[pa06]
    a[posa[3]] <- NA
    b[posb[3]] <- NA
    # player a reveals her next two numbers
    pa07 <- pickerJustMax(active = a, revealed = b[c(pb04, pb05)[-pa06]], type = "reveal")
    pa07 == 3
    pa08 <- pickerJustMax(active = a, revealed = b[c(pb04, pb05)[-pa06]], exclude = pa07, type = "reveal")
    pa08 == 2
    # player b selects one of player a's revealed numbers to match with his revealed number
    pb07 <- pickerRandom(active = b[c(pb04, pb05)[-pa06]], revealed = a[c(pa07, pa08)])
    pb07 == 1
    posa[4] <- c(pa07, pa08)[pb07]
    posb[4] <- c(pb04, pb05)[-pa06]
    a[posa[4]] <- NA
    b[posb[4]] <- NA
    # the remaining two numbers are matched.
    posa[5] <- c(pa07, pa08)[-pb07]
    posb[5] <- (1:5)[-c(posb)]
    
    # player a wins 2 to 1
    res <- c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    set.seed(13456)
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerJustMax, stratb = pickerRandom),
        expected = res)
    
})


