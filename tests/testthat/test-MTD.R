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

test_that(".mtd_multi_fields works", {
    res <- .mtd_multi_fields(as.list(c("homo_sapiens", "mus_musculus")),
                             prefix = "sample", suffix = "species")
    expect_equal(res[, 1L], c("sample[1]-species[1]", "sample[2]-species[1]"))
    expect_equal(res[, 2L], c("homo_sapiens", "mus_musculus"))

    res <- .mtd_multi_fields(list(c("mus_musculus", "homo_sapiens"), NULL,
                                  "rattus_norvegicus"),
                             prefix = "sample", suffix = "species")
    expect_equal(res[, 1L], c("sample[1]-species[1]", "sample[1]-species[2]",
                              "sample[3]-species[1]"))
    expect_equal(res[, 2L], c("mus_musculus", "homo_sapiens",
                              "rattus_norvegicus"))
    expect_equal(res[, 3L], c("1", "1", "3"))
})

test_that("mtd_sample works", {
    res <- mtd_sample(sample = c("a", "b", "c"))
    expect_equal(res[, 1L], c("sample[1]", "sample[2]", "sample[3]"))
    expect_equal(res[, 2L], c("a", "b", "c"))

    ## species
    res <- mtd_sample(sample = c("a", "b", "c"), species = "b")
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-species[1]",
                              "sample[2]", "sample[2]-species[1]",
                              "sample[3]", "sample[3]-species[1]"))
    expect_equal(res[, 2L], c("a", "b", "b", "b", "c", "b"))
    res <- mtd_sample(sample = c("a", "b", "c"),
                      species = list(c("A", "B"), NULL, 3))
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-species[1]",
                              "sample[1]-species[2]", "sample[2]",
                              "sample[3]", "sample[3]-species[1]"))
    expect_equal(res[, 2L], c("a", "A", "B", "b", "c", "3"))
    ## tissue
    res <- mtd_sample(sample = c("a", "b", "c"), tissue = "A")
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-tissue[1]",
                              "sample[2]", "sample[2]-tissue[1]",
                              "sample[3]", "sample[3]-tissue[1]"))
    res <- mtd_sample(sample = c("a", "b", "c"),
                      tissue = list(c("B"), NULL, 3:5))
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-tissue[1]",
                              "sample[2]", "sample[3]",
                              "sample[3]-tissue[1]", "sample[3]-tissue[2]",
                              "sample[3]-tissue[3]"))
    expect_equal(res[, 2L], c("a", "B", "b", "c", "3", "4", "5"))
    ## cell_type
    res <- mtd_sample(sample = c("a", "b", "c"), cell_type = "A")
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-cell_type[1]",
                              "sample[2]", "sample[2]-cell_type[1]",
                              "sample[3]", "sample[3]-cell_type[1]"))
    res <- mtd_sample(sample = c("a", "b", "c"),
                      cell_type = list(c("B"), NULL, 3:5))
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-cell_type[1]",
                              "sample[2]", "sample[3]",
                              "sample[3]-cell_type[1]", "sample[3]-cell_type[2]",
                              "sample[3]-cell_type[3]"))
    expect_equal(res[, 2L], c("a", "B", "b", "c", "3", "4", "5"))

    ## disease
    res <- mtd_sample(sample = c("a", "b", "c"), disease = "A")
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-disease[1]",
                              "sample[2]", "sample[2]-disease[1]",
                              "sample[3]", "sample[3]-disease[1]"))
    res <- mtd_sample(sample = c("a", "b", "c"),
                      disease = list(c("B"), NULL, 3:5))
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-disease[1]",
                              "sample[2]", "sample[3]",
                              "sample[3]-disease[1]", "sample[3]-disease[2]",
                              "sample[3]-disease[3]"))
    expect_equal(res[, 2L], c("a", "B", "b", "c", "3", "4", "5"))

    ## description
    expect_error(mtd_sample(sample = c("a", "b", "c"), description = "A"),
                 "length equal to")
    res <- mtd_sample(sample = c("a", "b", "c"), description = 1:3)
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-description",
                              "sample[2]", "sample[2]-description",
                              "sample[3]", "sample[3]-description"))
    expect_equal(res[, 2L], c("a", "1", "b", "2", "c", "3"))

    ## ...
    expect_error(mtd_sample(sample = c("a", "b", "c"), "A"),
                 "length has to match")
    res <- mtd_sample(sample = c("a", "b", "c"), description = 1:3,
                      c("custom 1", "custom 2", "custom 3"),
                      c("other 1", "other 2", "other 3"))
    expect_equal(res[, 1L], c("sample[1]", "sample[1]-description",
                              "sample[1]-custom[1]", "sample[1]-custom[2]",
                              "sample[2]", "sample[2]-description",
                              "sample[2]-custom[1]", "sample[2]-custom[2]",
                              "sample[3]", "sample[3]-description",
                              "sample[3]-custom[1]", "sample[3]-custom[2]"))
    expect_equal(res[, 2L], c("a", "1", "custom 1", "other 1",
                              "b", "2", "custom 2", "other 2",
                              "c", "3", "custom 3", "other 3"))
})

test_that("mtd_assay works", {
    res <- mtd_assay()
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_true(nrow(res) == 0)

    expect_error(mtd_assay(assay = c("first")), "is required")
    expect_error(mtd_assay(assay = c("first", "second"),
                           ms_run_ref = "ms_run[1]"), "have to match")

    res <- mtd_assay(assay = c("a", "b", "c"),
                     ms_run_ref = c("ms_run[1]", "ms_run[1]", "ms_run[2]"))
    expect_equal(
        res[, 1L],
        c("assay[1]", "assay[1]-ms_run_ref",
          "assay[2]", "assay[2]-ms_run_ref",
          "assay[3]", "assay[3]-ms_run_ref"))
    expect_equal(
        res[, 2L],
        c("a", "ms_run[1]",
          "b", "ms_run[1]",
          "c", "ms_run[2]"))

    res <- mtd_assay(assay = c("a", "b", "c"),
                     external_uri = c("B"),
                     ms_run_ref = c("ms_run[1]", "ms_run[1]", "ms_run[2]"))
    expect_equal(
        res[, 1L],
        c("assay[1]", "assay[1]-external_uri", "assay[1]-ms_run_ref",
          "assay[2]", "assay[2]-external_uri", "assay[2]-ms_run_ref",
          "assay[3]", "assay[3]-external_uri", "assay[3]-ms_run_ref"))
    expect_equal(
        res[, 2L],
        c("a", "B", "ms_run[1]",
          "b", "B", "ms_run[1]",
          "c", "B", "ms_run[2]"))
    
    expect_error(mtd_assay(assay = c("a", "b"),
                           sample_ref = c("sample[1]"),
                           ms_run_ref = c("ms_run[1]", "b")),
                 "has to match")
    res <- mtd_assay(assay = "a", ms_run_ref = "b", sample_ref = "B")
    expect_equal(res[, 1L],
                 c("assay[1]", "assay[1]-sample_ref", "assay[1]-ms_run_ref"))
    expect_equal(res[, 2L], c("a", "B", "b"))
    
    res <- mtd_assay(assay = c("a", "b"), ms_run_ref = c("1", "2"),
                     a = 1:2, b = 3:4)
    expect_equal(
        res[, 1L],
        c("assay[1]", "assay[1]-ms_run_ref",
          "assay[1]-custom[1]", "assay[1]-custom[2]",
          "assay[2]", "assay[2]-ms_run_ref",
          "assay[2]-custom[1]", "assay[2]-custom[2]"))
    expect_equal(
        res[, 2L],
        c("a", "1", "1", "3", "b", "2", "2", "4"))
    
    ## multi assignment assay->ms_run
    expect_error(mtd_assay(assay = c("a", "b"), ms_run_ref = list(1:2, NULL)),
                 "At least one")
    res <- mtd_assay(assay = c("a", "b"), ms_run_ref = list(1:2, 3))
    expect_equal(
        res[, 1L],
        c("assay[1]", "assay[1]-ms_run_ref[1]", "assay[1]-ms_run_ref[2]",
          "assay[2]", "assay[2]-ms_run_ref[1]"))
    expect_equal(res[, 2L], c("a", "1", "2", "b", "3"))
})

test_that(".mtd_custom_fields works", {
    res <- .mtd_custom_fields()
    expect_true(is.matrix(res))
    expect_true(is.character(res))
    expect_true(nrow(res) == 0)

    expect_error(.mtd_custom_fields(1:3, c("a", "b"), expected_length = 3L),
                 "length has to match the length")

    res <- .mtd_custom_fields(1:3, c("a", "b", "c"), expected_length = 3L)
    expect_equal(
        res[, 1L],
        c("sample[1]-custom[1]", "sample[2]-custom[1]", "sample[3]-custom[1]",
          "sample[1]-custom[2]", "sample[2]-custom[2]", "sample[3]-custom[2]"))
    expect_equal(res[, 2L], c("1", "2", "3", "a", "b", "c"))
    expect_equal(res[, 3L], c("1", "2", "3", "1", "2", "3"))
})

test_that("mtd_study_variables works", {
    x <- data.frame(
        name = c("I1_0", "I2_0", "I1_6", "I2_6", "I3_0"),
        individual = c("I1", "I2", "I1", "I2", "I3"),
        timepoint = c("0h", "6h", "0h", "6h", "0h"),
        T2D = c(TRUE, FALSE, TRUE, FALSE, FALSE)
    )
    expect_error(
        mtd_study_variables(x, study_variable_columns = c("T2D"),factors = 1:3),
        "currently not supported")
    expect_error(
        mtd_study_variables(x, study_variable_columns = c("T2D", "A")),
        "Not all column names")
    
    res <- mtd_study_variables(x, average_function = "A",
                               variation_function = "B")
    expect_equal(res[, 1L], c("study_variable[1]",
                              "study_variable[1]-assay_refs",
                              "study_variable[1]-average_function",
                              "study_variable[1]-variation_function",
                              "study_variable[1]-description"))
    expect_equal(res[, 2L], c("undefined",
                              "assay[1]|assay[2]|assay[3]|assay[4]|assay[5]",
                              "A",
                              "B",
                              "Undefined"))

    expect_error(
        mtd_study_variables(x, average_function = character(),
                            variation_function = "B"), "'average_function'")
    expect_error(
        mtd_study_variables(x, average_function = "A",
                            variation_function = NULL), "'variation_function'")
    expect_error(
        mtd_study_variables(x, study_variable_columns = c("T2D", "timepoint"),
                            average_function = "A", variation_function = "B",
                            description = 1:2), "'description'")
    
    res <- mtd_study_variables(
        x, study_variable_columns = c("T2D", "timepoint"),
        average_function = "A", variation_function = "B")
    expect_equal(res[, 1L],
                 c("study_variable[1]",
                   "study_variable[1]-assay_refs",
                   "study_variable[1]-average_function",
                   "study_variable[1]-variation_function",
                   "study_variable[1]-description",
                   "study_variable[2]",
                   "study_variable[2]-assay_refs",
                   "study_variable[2]-average_function",
                   "study_variable[2]-variation_function",
                   "study_variable[2]-description",
                   "study_variable[3]",
                   "study_variable[3]-assay_refs",
                   "study_variable[3]-average_function",
                   "study_variable[3]-variation_function",
                   "study_variable[3]-description",
                   "study_variable[4]",
                   "study_variable[4]-assay_refs",
                   "study_variable[4]-average_function",
                   "study_variable[4]-variation_function",
                   "study_variable[4]-description"))
    expect_equal(res[, 2L],
                 c("T2D:TRUE",
                   "assay[1]|assay[3]",
                   "A",
                   "B",
                   "Column: T2D, value: TRUE",
                   "T2D:FALSE",
                   "assay[2]|assay[4]|assay[5]",
                   "A",
                   "B",
                   "Column: T2D, value: FALSE",
                   "timepoint:0h",
                   "assay[1]|assay[3]|assay[5]",
                   "A",
                   "B",
                   "Column: timepoint, value: 0h",
                   "timepoint:6h",
                   "assay[2]|assay[4]",
                   "A",
                   "B",
                   "Column: timepoint, value: 6h"))
})
