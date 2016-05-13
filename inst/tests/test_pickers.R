

context("pick first piece")

test_that("normal case", {
    
    expect_equal(object = pickerFirst(revealed = 1:2),
        expected = 1)
    
    expect_equal(object = pickerFirst(revealed = 1),
        expected = 1)
    
    expect_equal(object = pickerFirst(
            active = rep(1, 5), 
            revealed = rep(1, 5), 
            type = "reveal"),
        expected = 1)
    
    expect_equal(object = pickerFirst(
            active = 1, 
            revealed = 1, 
            exclude = 1, 
            type = "reveal"),
        expected = NA_real_)
    
    expect_equal(object = pickerFirst(
        active = 10, 
        revealed = 10:14, 
        exclude = 1, 
        type = "reveal"),
        expected = NA_real_)
    
    expect_equal(object = pickerFirst(
        active = c(NA, 10:14), 
        revealed = 10, 
        exclude = 1:2, 
        type = "reveal"),
        expected = 3)
    
    expect_equal(object = pickerFirst(
        active = 10, 
        revealed = c(NA, 10:14), 
        exclude = 1:2, 
        type = "pair"),
        expected = 2)
    
})




context("pick random piece")

test_that("normal case", {
    
    set.seed(35246)
    expect_equal(object = pickerRandom(revealed = 1:2),
        expected = 1)
    
    set.seed(32246)
    expect_equal(object = pickerRandom(revealed = 3:4),
        expected = 2)
    
    set.seed(35241)
    expect_equal(object = pickerRandom(revealed = 1),
        expected = 1)
    
    set.seed(35216)
    expect_equal(object = pickerRandom(
            active = rep(1, 5), 
            revealed = rep(1, 5), 
            type = "reveal"),
        expected = 5)
    
    set.seed(35216)
    expect_equal(object = pickerRandom(
        active = c(5, 1:4), 
        revealed = 2, 
        type = "reveal"),
        expected = 5)
    
    expect_equal(object = pickerRandom(
            active = 1, 
            revealed = 1, 
            exclude = 1, 
            type = "reveal"),
        expected = NA_real_)
    
    set.seed(35246)
    expect_equal(object = pickerRandom(active = c(5, 1:4), 
        revealed = 2, exclude = 1, type = "reveal"),
        expected = 3)
    
    set.seed(35246)
    expect_equal(object = pickerRandom(active = 1:5, 
            revealed = 1:5, type = "reveal"),
        expected = 3)
    
    set.seed(15242)
    expect_equal(object = pickerRandom(active = c(4L, NA, 2L, NA, NA), 
            revealed = 4, type = "reveal"),
        expected = 1)
})


context("pick piece for max win")

test_that("normal case", {
    
    expect_equal(object = pickerMax(active = 2, revealed = c(3, 1)),
        expected = 2)
    
    expect_equal(object = pickerMax(active = 1, revealed = 1:2),
        expected = 1)
    
    expect_equal(object = pickerMax(active = 1, revealed = 1),
        expected = 1)
    
    expect_equal(object = pickerMax(active = 4, 
            revealed = 4, 
            exclude = 1),
        expected = 1)
    
    expect_equal(object = pickerMax(
            active = rep(1, 5), 
            revealed = rep(1, 5), 
            type = "reveal"),
        expected = 1)
    
    expect_equal(object = pickerMax(
            active = 1, 
            revealed = 1, 
            exclude = 1, 
            type = "reveal"),
        expected = NA_integer_)
    
    expect_equal(object = pickerMax(active = 2, 
            revealed = c(3, 1), 
            type = "reveal"),
        expected = 1)
    
    expect_equal(object = pickerMax(active = 1:5, 
            revealed = c(3, 1), 
            type = "reveal"),
        expected = 5)
    
    expect_equal(object = pickerMax(active = rep(5, 5), 
            revealed = 1:5, 
            type = "reveal"),
        expected = 1)
    
    expect_equal(object = pickerMax(active = rep(5, 5), 
            revealed = 1:5, 
            exclude = 1, 
            type = "reveal"),
        expected = 2)
    
    expect_equal(object = pickerMax(active = 5, 
        revealed = c(5, 1:4), 
        exclude = 1, 
        type = "reveal"),
        expected = NA_real_)
    
    expect_equal(object = pickerMax(active = 3, 
        revealed = c(3, 2)),
        expected = 2)
})



context("pick piece to squeak win")

test_that("normal case", {
    
    expect_equal(object = pickerJustMax(active = 2, revealed = c(3, 1)),
        expected = 1)
    
    # take one for team
    expect_equal(object = pickerJustMax(active = 1, revealed = 1:2),
        expected = 2)
    
    expect_equal(object = pickerJustMax(active = 1, revealed = 1),
        expected = 1)
    
    expect_equal(object = pickerJustMax(active = 4, 
            revealed = 4, 
            exclude = 1),
        expected = 1)
    
    # prefer draws over defeats
    expect_equal(object = pickerJustMax(active = 3, 
        revealed = 2:3),
        expected = 2)
    
    expect_equal(object = pickerJustMax(
            active = rep(1, 5), 
            revealed = rep(1, 5), 
            type = "reveal"),
        expected = 1)
    
    expect_equal(object = pickerJustMax(
            active = 1, 
            revealed = 1, 
            exclude = 1, 
            type = "reveal"),
        expected = NA_integer_)
    
    expect_equal(object = pickerJustMax(active = 2, 
            revealed = c(3, 1), 
            type = "reveal"),
        expected = 1)
    
    expect_equal(object = pickerJustMax(active = 2:1, 
            revealed = 3:4, 
            type = "reveal"),
        expected = 2)
    
    expect_equal(object = pickerJustMax(active = 1:5, 
            revealed = c(3, 1), 
            type = "reveal"),
        expected = 4)
    
    expect_equal(object = pickerJustMax(active = rep(5, 5), 
            revealed = 1:5, 
            type = "reveal"),
        expected = 5)
    
    expect_equal(object = pickerJustMax(active = rep(5, 5), 
            revealed = 1:5, 
            exclude = 1, 
            type = "reveal"),
        expected = 5)
    
    expect_equal(object = pickerJustMax(active = c(4L, NA, 2L, NA, NA), 
        revealed = 2, 
        exclude = 1, 
        type = "reveal"),
        expected = 3)
    
    expect_equal(object = pickerJustMax(active = c(NA, 1, NA, NA, 3), 
        revealed = 3, 
        exclude = 5, 
        type = "reveal"),
        expected = 2)
})


