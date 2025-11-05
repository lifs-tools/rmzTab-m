test_that(".cv works", {
    res <- .cv()
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_true(nrow(res) == 0)

    expect_error(.cv("a"), "different number")

    res <- .cv(c("a", "b", "c"), c("A", "B", "C"), 1:3, 1:3)
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_true(nrow(res) == 12)
    expect_equal(
        res[, 1],
        c("cv[1]-label", "cv[1]-full_name", "cv[1]-version", "cv[1]-uri",
          "cv[2]-label", "cv[2]-full_name", "cv[2]-version", "cv[2]-uri",
          "cv[3]-label", "cv[3]-full_name", "cv[3]-version", "cv[3]-uri"))
    expect_equal(res[, 2], c("a", "A", "1", "1",
                             "b", "B", "2", "2",
                             "c", "C", "3", "3"))
})
