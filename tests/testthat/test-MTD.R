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

test_that(".ms_run_format works", {
    res <- .ms_run_format(1:4, "test", c("A", "B", "C", "D"))
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_equal(nrow(res), 4)
    expect_equal(ncol(res), 3)
    expect_equal(res[, 1L], c("ms_run[1]-test", "ms_run[2]-test",
                              "ms_run[3]-test", "ms_run[4]-test"))
    expect_equal(res[, 2L], c("A", "B", "C", "D"))
    expect_equal(res[, 3L], as.character(1:4))
})

test_that(".ms_scan_polarity works", {
    expect_error(.ms_scan_polarity(c("positive", "other")), "has to be")
    res <- .ms_scan_polarity(c("positive", "negative"))
    expect_equal(res, c("[MS, MS:1000130, positive scan, ]",
                        "[MS, MS:1000129, negative scan, ]"))
})

test_that("mtd_ms_run works", {
    expect_error(mtd_ms_run(), "'location' is required")
    expect_error(mtd_ms_run(location = "null", "'scan_polarity' is required"))
    expect_error(mtd_ms_run(location = c("null", "other"),
                            scan_polarity = c("positive", "negative"),
                            format = "a"), "have to be defined")
    expect_error(mtd_ms_run(location = c("null", "other"),
                            scan_polarity = c("positive", "negative"),
                            format = "a", id_format = 1:2),
                 "have to be defined")
    expect_error(mtd_ms_run(location = c("null", "other"),
                            scan_polarity = c("positive", "negative"),
                            hash = "a"), "have to be defined")
    expect_error(mtd_ms_run(location = c("null", "other"),
                            scan_polarity = c("positive", "negative"),
                            hash = "a", hash_method = "a"), "match length")
    expect_error(mtd_ms_run(location = c("null", "other"),
                            scan_polarity = c("positive", "negative"),
                            fragmentation_method = list(3)), "match length")
    res <- mtd_ms_run(location = c("null", "other"),
                      scan_polarity = c("positive", "negative"))
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_equal(res[, 1L], c("ms_run[1]-location",
                              "ms_run[1]-scan_polarity[1]",
                              "ms_run[2]-location",
                              "ms_run[2]-scan_polarity[1]"))
    expect_equal(res[, 2L], c("null",
                              "[MS, MS:1000130, positive scan, ]",
                              "other",
                              "[MS, MS:1000129, negative scan, ]"))
    ## instrument_ref
    res <- mtd_ms_run(location = c("null", "other"),
                      scan_polarity = c("positive", "negative"),
                      instrument_ref = 1)
    expect_true(nrow(res) == 6)
    expect_equal(res[, 1L], c("ms_run[1]-location",
                              "ms_run[1]-instrument_ref",
                              "ms_run[1]-scan_polarity[1]",
                              "ms_run[2]-location",
                              "ms_run[2]-instrument_ref",
                              "ms_run[2]-scan_polarity[1]"))
    expect_equal(res[, 2L], c("null",
                              "instrument[1]",
                              "[MS, MS:1000130, positive scan, ]",
                              "other",
                              "instrument[1]",
                              "[MS, MS:1000129, negative scan, ]"))
    ## format
    res <- mtd_ms_run(location = c("null", "other"),
                      scan_polarity = c("positive", "negative"),
                      format = "[MS, MS:1000584, mzML file, ]",
                      id_format = "[MS, MS:1000530, mzML unique identifier, ]")
    expect_equal(res[, 1L], c("ms_run[1]-location",
                              "ms_run[1]-format",
                              "ms_run[1]-id_format",
                              "ms_run[1]-scan_polarity[1]",
                              "ms_run[2]-location",
                              "ms_run[2]-format",
                              "ms_run[2]-id_format",
                              "ms_run[2]-scan_polarity[1]"))
    expect_equal(res[, 2L], c("null",
                              "[MS, MS:1000584, mzML file, ]",
                              "[MS, MS:1000530, mzML unique identifier, ]",
                              "[MS, MS:1000130, positive scan, ]",
                              "other",
                              "[MS, MS:1000584, mzML file, ]",
                              "[MS, MS:1000530, mzML unique identifier, ]",
                              "[MS, MS:1000129, negative scan, ]"))
    ## hash
    res <- mtd_ms_run(location = c("null", "other"),
                      scan_polarity = c("positive", "negative"),
                      hash = c("a", "b"),
                      hash_method = c("[MS, MS:1000569, SHA-1, ]",
                                      "[MS, MS:1000569, SHA-1, ]"))
    expect_equal(res[, 1L], c("ms_run[1]-location",
                              "ms_run[1]-scan_polarity[1]",
                              "ms_run[1]-hash",
                              "ms_run[1]-hash_method",
                              "ms_run[2]-location",
                              "ms_run[2]-scan_polarity[1]",
                              "ms_run[2]-hash",
                              "ms_run[2]-hash_method"))
    expect_equal(res[, 2L], c("null",
                              "[MS, MS:1000130, positive scan, ]",
                              "a",
                              "[MS, MS:1000569, SHA-1, ]",
                              "other",
                              "[MS, MS:1000129, negative scan, ]",
                              "b",
                              "[MS, MS:1000569, SHA-1, ]"))
    ## fragmentation_method
    res <- mtd_ms_run(location = c("null", "other"),
                      scan_polarity = c("positive", "negative"),
                      fragmentation_method = list(NULL,
                                                  c("[MS, MS:1000133, CID, ]",
                                                    "[MS, MS:1000422, HCD, ]")))
    expect_equal(res[, 1L], c("ms_run[1]-location",
                              "ms_run[1]-scan_polarity[1]",
                              "ms_run[2]-location",
                              "ms_run[2]-fragmentation_method[1]",
                              "ms_run[2]-fragmentation_method[2]",
                              "ms_run[2]-scan_polarity[1]"))
    expect_equal(res[, 2L], c("null",
                              "[MS, MS:1000130, positive scan, ]",
                              "other",
                              "[MS, MS:1000133, CID, ]",
                              "[MS, MS:1000422, HCD, ]",
                              "[MS, MS:1000129, negative scan, ]"))
})
