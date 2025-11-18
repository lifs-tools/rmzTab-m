test_that(".prefix_zero works", {
    expect_equal(.prefix_zero(54), "54")
    expect_equal(.prefix_zero(c(1, 100)), c("001", "100"))
})
