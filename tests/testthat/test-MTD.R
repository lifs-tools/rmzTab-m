test_that(".cv works", {
    res <- .cv()
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_true(nrow(res) == 0)

    expect_error(.cv("a"), "elements must match")

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

test_that(".database works", {
    res <- .database()
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_true(nrow(res) == 0)
    
    expect_error(.database("a"), "elements must match")
    res <- .database(c("a", "b"), c("ap", "bp"),
                     version = c(1, "null"), uri = c("A", "B"))
    expect_equal(res[, 1L], c("database[1]",
                              "database[1]-prefix",
                              "database[1]-version",
                              "database[1]-uri",
                              "database[2]",
                              "database[2]-prefix",
                              "database[2]-version",
                              "database[2]-uri"))
    expect_equal(res[, 2L], c("a", "ap", "1", "A", "b", "bp", "null", "B"))
})

test_that("mtd_skeleton works", {
    expect_error(mtd_skeleton(), "'id' is required")
    expect_error(mtd_skeleton(id = "1"), "'software' is required")
    res <- mtd_skeleton(id = "1", software = "Fancy software")
    expect_true(is.matrix(res))
    expect_true(is.character(res))
})

test_that("mtd_sort works", {
    ref <- mtd_skeleton(id = "a", software = "Excel")
    res <- mtd_sort(ref)
    expect_equal(ref, res)

    ref <- ref[1:19, ]
    res <- mtd_sort(ref[c(5:12, 1, 3, 13:16, 2, 4, 18, 19, 17), ])
    expect_equal(ref, res)
})
