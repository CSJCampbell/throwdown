

context("play pairingGame")

test_that("normal case", {
    
    expect_equal(object = pairingGame(a = 1:5, b = 5:1),
        expected = structure(c(2L, 2L), .Names = c("a", "b")))
    
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
        expected = structure(c(2L, 2L), .Names = c("a", "b")))
    
    set.seed(22132)
    expect_equal(object = pairingGame(a = 1:5, b = c(5, 1:4), 
            strata = pickerRandom, stratb = pickerRandom),
        expected = structure(c(2L, 2L), .Names = c("a", "b")))
    
    set.seed(22513)
    expect_equal(object = pairingGame(a = 1:5, b = c(5, 1:4), 
            strata = pickerRandom, stratb = pickerMax),
        expected = structure(c(2L, 2L), .Names = c("a", "b")))
    
    set.seed(22531)
    expect_equal(object = pairingGame(a = 1:5, b = c(5, 1:4), 
            strata = pickerRandom, stratb = pickerJustMax),
        expected = structure(c(2L, 2L), .Names = c("a", "b")))
    
    set.seed(12532)
    expect_equal(object = pairingGame(a = sample(1:5, size = 5), b = sample(1:5, size = 5), 
            strata = pickerRandom, stratb = pickerRandom),
        expected = structure(c(3L, 2L), .Names = c("a", "b")))
    
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
    
    # player a wins 3 to 2
    c(a = sum(aa[posa] > bb[posb]), 
        b = sum(aa[posa] < bb[posb]))
    
    expect_equal(object = pairingGame(a = aa, 
        b = bb, 
        strata = pickerJustMax, stratb = pickerMax),
        expected = structure(c(2L, 2L), .Names = c("a", "b")))
})


