test_that("compute_NPV_works", {

expect_equal(compute_NPV(value=0,time=100,discount=0.1), 0)
expect_equal(compute_NPV(value=100, time=0, discount=0.1), 100)
})

